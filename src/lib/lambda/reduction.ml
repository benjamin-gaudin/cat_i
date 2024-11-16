open Common.Ast
open Error

let rec lift_rec d t =
  match t with
  | Cst c           -> Cst c
  | Nat n           -> Nat n
  | Lbl s           -> Lbl s
  | Var n           -> if n <= d then t else Var (n + 1)
  | Rcd ts          -> Rcd (List.map (fun (l, t) -> (l, lift_rec d t)) ts)
  | Vrt ts          -> Vrt (List.map (fun (l, t) -> (l, lift_rec d t)) ts)
  | As  (t, ty)     -> As  (lift_rec d t, ty)
  | Uop (Abs, t)    -> Uop (Abs, (lift_rec (d + 1) t))
  | Uop (u, t)      -> Uop (u, (lift_rec d t))
  | Bop (b, t1, t2) -> Bop (b, (lift_rec d t1), (lift_rec d t2))
  | Let (x, t1, t2) -> Let ((lift_rec d x), (lift_rec d t1), (lift_rec d t2))
  | Cod (cond, c, t1, t2) ->
      Cod (cond, (lift_rec d c), (lift_rec d t1), (lift_rec d t2))
  | Cas (t, ts)     -> Cas
    (lift_rec d t, List.map (fun (l, t) -> (l, lift_rec (d + 1) t)) ts)

let lift = lift_rec 0


let rec subs_rec d t x u =
  match t with
  | Cst c           -> Cst c
  | Nat n           -> Nat n
  | Lbl s           -> Lbl s
  | Var n           -> if n = d + x then u else t
  | Rcd ts          -> Rcd (List.map (fun (l, t) -> (l, subs_rec d t x u)) ts)
  | Vrt ts          -> Vrt (List.map (fun (l, t) -> (l, subs_rec d t x u)) ts)
  | As  (t, ty)     -> As  (subs_rec d t x u, ty)
  | Uop (Abs, t')   -> Uop (Abs, (subs_rec (d + 1) t' x (lift u)))
  | Uop (uop, t)    -> Uop (uop, (subs_rec d t x u))
  | Bop (b, t1, t2) -> Bop (b, (subs_rec d t1 x u), (subs_rec d t2 x u))
  | Let (v, t1, t2) ->
      Let (subs_rec d v x u, subs_rec d t1 x u, subs_rec d t2 x u)
  | Cod (cond, c, t1, t2) -> 
      Cod (cond, subs_rec d c x u, subs_rec d t1 x u, subs_rec d t2 x u)
  | Cas (t, ts) -> Cas
    (subs_rec d t x u, List.map (fun (l, t) -> (l, subs_rec (d + 1) t x u)) ts)

let subs = subs_rec 0


(* Return if a term is a value aka a non-reductible term *)
let rec is_value = function
  | Cst _ | Nat _ | Var _ | Uop (Abs, _) | Bop (App, Cst _, _) |
      Bop (App, Var _, _) | Lbl _ -> true 
  | Bop (Con, t, ts) when is_value t && is_value ts -> true
  | Rcd ts when List.for_all (fun (_, t) -> is_value t) ts -> true
  | Vrt ts when List.for_all (fun (_, t) -> is_value t) ts -> true
  | Uop _ | Bop _ | Let _ | Cod _ | Rcd _ | Cas _ | As _ | Vrt _ -> false

let rec red_step t = 
  match t with
  | Rcd _ -> red_step_Rcd t
  | As  _ -> red_step_As  t
  | Uop _ -> red_step_Uop t
  | Bop _ -> red_step_Bop t
  | Let _ -> red_step_Let t
  | Cod _ -> red_step_Cod t
  | Cas _ -> red_step_Cas t
  | _     -> None

and red_step_Rcd = function
  | Rcd ts when not (is_value (Rcd ts)) -> 
      Some (Rcd (List.map
      (fun (l, t) -> (match red_step t with
                 | Some t' -> (l, t')
                 | None    -> (l, t)
                )) ts))
  | _ -> None

and red_step_As = function
  | As (t, _) -> Some t
  | _         -> None

and red_step_Uop = function
  | Uop (HD, Bop (Con, t, _))  -> Some t
  | Uop (TL, Bop (Con, _, ts)) -> Some ts
  | Uop (Fix, Uop (Abs, t1))   -> Some (subs t1 0 (Uop (Fix, Uop (Abs, t1))))
  | Uop (u, t) when not (is_value t) -> 
      Option.bind (red_step t) (fun t -> Some (Uop (u, t)))
  | _                          -> None

and red_step_Bop = function
  | Bop (And, Cst Tru, b) | Bop (And, b, Cst Tru) -> Some b
  | Bop (And, Cst Fal, _) | Bop (And, _, Cst Fal) -> Some (Cst Fal)
  | Bop (Or , Cst Tru, _) | Bop (Or , _, Cst Tru) -> Some (Cst Tru)
  | Bop (Or , Cst Fal, b) | Bop (Or , b, Cst Fal) -> Some b
  | Bop (Sub, Nat 0, Nat n) | Bop (Sub, Nat n, Nat 0) -> Some (Nat n)
  | Bop (Add, Nat m, Nat n) -> Some (Nat (m + n))
  | Bop (Sub, Nat m, Nat n) -> Some (Nat (m - n))
  | Bop (Mul, Nat m, Nat n) -> Some (Nat (m * n))
  | Bop (Prj, Nat n, Rcd ts) when n < List.length ts -> Some (List.nth ts n |> snd)
  (* | Bop (Prj, Nat _, Rcd _) -> failwith "TODO projection with integer too high" *)
  | Bop (Prj, Lbl s, Rcd ts) -> 
      (try 
        Some (List.assoc s ts)
       with Not_found -> eraise (EPrjLabelNotFound (s, Rcd ts)))
  | Bop (App, Uop (Abs, t1), t2) when is_value t2 -> Some (subs t1 0 t2)
  | Bop (b,   t1,    t2   ) when not (is_value t1) -> 
      Option.bind (red_step t1) (fun t -> Some (Bop (b, t, t2)))
  | Bop (b,   t1,    t2   ) when not (is_value t2) ->
      Option.bind (red_step t2) (fun t -> Some (Bop (b, t1, t)))
  | _               -> None

and red_step_Cod = function
  | Cod (If,  Cst Tru, t1, _) -> Some t1
  | Cod (If,  Cst Fal, _, t2) -> Some t2
  | Cod (Ifn, Cst Nil, t1, _) -> Some t1
  | Cod (Ifz, Nat 0,   t1, _) -> Some t1
  | Cod (Ifz, Nat _,   _, t2) -> Some t2
  | Cod (Ifn, Bop (Con, _, _),   _, t2) -> Some t2
  | Cod (co, c, t1, t2) when not (is_value c) ->
      Option.bind (red_step c) (fun t -> Some (Cod (co, t, t1, t2)))
  | _                         -> None

and red_step_Let = function
  | Let (x, t1, t2) when not (is_value x) ->
      Option.bind (red_step x) (fun t -> Some (Let (t, t1, t2)))
  | Let (x, t1, t2) when not (is_value t1) ->
      Option.bind (red_step t1) (fun t -> Some (Let (x, t, t2)))
  | Let (Var n, t1, t2) -> Some (subs t2 n t1)
  | _               -> None

and red_step_Cas = function
  | Cas (t, ts) when not (is_value t) ->
      Option.bind (red_step t) (fun t -> Some (Cas (t, ts)))
  | Cas (Vrt [(l, t)], ts) -> Some (subs (List.assoc l ts) 0 t)
  | _                      -> None

(* Retourne the normal form of a term *)
let rec norm_rec n t =
  if n > 500000 then None else
  (* Option.fold ~none:(Some t) ~some:(norm_rec (n+1)) (red_step t)) *)
    match red_step t with
    | Some t' -> norm_rec (n + 1) t'
    | None    -> Some t

let norm = norm_rec 0


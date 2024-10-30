open Common.Ast

let rec lift_rec d t =
  match t with
  | Cst c           -> Cst c
  | Nat n           -> Nat n
  | Var n           -> if n <= d then t else Var (n + 1)
  | Tpl ts          -> Tpl (List.map (lift_rec d) ts)
  | Uop (Abs, t)    -> Uop (Abs, (lift_rec (d + 1) t))
  | Uop (u, t)      -> Uop (u, (lift_rec d t))
  | Bop (b, t1, t2) -> Bop (b, (lift_rec d t1), (lift_rec d t2))
  | Let (x, t1, t2) -> Let ((lift_rec d x), (lift_rec d t1), (lift_rec d t2))
  | Cod (cond, c, t1, t2) ->
      Cod (cond, (lift_rec d c), (lift_rec d t1), (lift_rec d t2))

let lift = lift_rec 0

let rec subs_rec d t x u =
  match t with
  | Cst c           -> Cst c
  | Nat n           -> Nat n
  | Var n           -> if n = d + x then u else t
  | Tpl ts          -> Tpl (List.map (fun t -> subs_rec d t x u) ts)
  | Uop (Abs, t')   -> Uop (Abs, (subs_rec (d + 1) t' x (lift u)))
  | Uop (uop, t)    -> Uop (uop, (subs_rec d t x u))
  | Bop (b, t1, t2) -> Bop (b, (subs_rec d t1 x u), (subs_rec d t2 x u))
  | Let (v, t1, t2) ->
      Let (subs_rec d v x u, subs_rec d t1 x u, subs_rec d t2 x u)
  | Cod (cond, c, t1, t2) -> 
      Cod (cond, subs_rec d c x u, subs_rec d t1 x u, subs_rec d t2 x u)

let subs = subs_rec 0

(* Return if a term is a value aka a non-reductible term *)
let rec is_value = function
  | Cst _ | Nat _ | Var _ | Uop (Abs, _) | Bop (App, Cst _, _) |
      Bop (App, Var _, _)  -> true
  | Bop (Con, t, ts) when is_value t && is_value ts -> true
  | Tpl ts when List.for_all is_value ts -> true
  | Uop _ | Bop _ | Let _ | Cod _ | Tpl _  -> false

let rec red_step t = 
  match t with
  | Tpl _ -> red_step_Tpl t
  | Uop _ -> red_step_Uop t
  | Bop _ -> red_step_Bop t
  | Let _ -> red_step_Let t
  | Cod _ -> red_step_Cod t
  | _     -> None

and red_step_Tpl = function
  | Tpl ts -> Some (Tpl (List.map
      (fun t -> (match red_step t with
                 | Some t' -> t'
                 | None    -> t
                )) ts))
  | _ -> None

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
  | Bop (Prj, Nat n, Tpl ts) when n < List.length ts -> Some (List.nth ts n)
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

(* Retourne the normal form of a term *)
let rec norm_rec n t =
  if n > 500000 then None else
  (* Option.fold ~none:(Some t) ~some:(norm_rec (n+1)) (red_step t)) *)
    match red_step t with
    | Some t' -> norm_rec (n + 1) t'
    | None    -> Some t

let norm = norm_rec 0


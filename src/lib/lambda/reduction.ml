open Common.Ast

let rec lift_list_rec d = function
  | Nil         -> Nil
  | Con (t, ts) -> Con (lift_rec d t, lift_list_rec d ts)

and lift_rec d t =
  match t with
  | Nat n           -> Nat n
  | Var n           -> if n <= d then t else Var (n + 1)
  | Lis l           -> Lis (lift_list_rec d l)
  | Uop (Abs, t)    -> Uop (Abs, (lift_rec (d + 1) t))
  | Uop (u, t)      -> Uop (u, (lift_rec d t))
  | Bop (b, t1, t2) -> Bop (b, (lift_rec d t1), (lift_rec d t2))
  | Let (x, t1, t2) -> Let ((lift_rec d x), (lift_rec d t1), (lift_rec d t2))
  | Cod (cond, c, t1, t2) ->
      Cod (cond, (lift_rec d c), (lift_rec d t1), (lift_rec d t2))

let lift = lift_rec 0

let rec subs_list_rec d l x u =
  match l with
  | Nil         -> Nil
  | Con (t, ts) -> Con (subs_rec d t x u, subs_list_rec d ts x u)

and subs_rec d t x u =
  match t with
  | Nat n           -> Nat n
  | Var n           -> if n = d + x then u else t
  | Lis l           -> Lis (subs_list_rec d l x u)
  | Uop (Abs, t')   -> Uop (Abs, (subs_rec (d + 1) t' x (lift u)))
  | Uop (uop, t)      -> Uop (uop, (subs_rec d t x u))
  | Bop (b, t1, t2) -> Bop (b, (subs_rec d t1 x u), (subs_rec d t2 x u))
  | Let (v, t1, t2) ->
      Let (subs_rec d v x u, subs_rec d t1 x u, subs_rec d t2 x u)
  | Cod (cond, c, t1, t2) -> 
      Cod (cond, subs_rec d c x u, subs_rec d t1 x u, subs_rec d t2 x u)

let subs = subs_rec 0

(* Return if a term is a value aka a non-reductible term *)
let is_value = function
  | Nat _ | Var _ | Lis _ | Uop (Abs, _) |
      Bop (App, (Var _), _) -> true
  | Uop _ | Bop _ | Let _ | Cod _ -> false


let rec red_step = function
  | Uop (HD, Lis Con (t, _))  -> Some t
  | Uop (TL, Lis Con (_, ts)) -> Some (Lis ts)
  (* | Bop (Add, Nat 0, Nat n) | Bop (Add, Nat n, Nat 0)
    | Bop (Sub, Nat 0, Nat n) | Bop (Sub, Nat n, Nat 0) -> Some (Nat n)
  | Bop (Mul, Nat 0, Nat _) | Bop (Mul, Nat _, Nat 0) -> Some (Nat 0) *)
  | Bop (Add, Nat m, Nat n) -> Some (Nat (m + n))
  | Bop (Sub, Nat m, Nat n) -> Some (Nat (m - n))
  | Bop (Mul, Nat m, Nat n) -> Some (Nat (m * n))
  | Bop (b, t1, t2 ) when not (b = App) ->
      Option.bind (red_step t1) (fun t -> Some (Bop (b, t, t2)))
  | Bop (b, t1, t2 ) when not (b = App) ->
      Option.bind (red_step t2) (fun t -> Some (Bop (b, t1, t)))
  | Cod (Ifn, Nat 0, t1, _) -> Some t1
  | Cod (Ifz, Lis Nil, t1, _) -> Some t1
  | Cod (co, c, t1, t2) when not (is_value c) ->
      Option.bind (red_step c) (fun t -> Some (Cod (co, t, t1, t2)))
  | Cod (Ifz, Nat _, _, t2) -> Some t2
  | Cod (Ifn, Lis Nil, _, t2) -> Some t2
  | Uop (Fix, Uop (Abs, t1)) -> Some (subs t1 0 (Uop (Fix, Uop (Abs, t1))))
  | Let (x, t1, t2) when not (is_value x) -> 
      Option.bind (red_step x) (fun t -> Some (Let (t, t1, t2)))
  | Let (x, t1, t2) when not (is_value t1) -> 
      Option.bind (red_step t1) (fun t -> Some (Let (x, t, t2)))
  | Let (Var n, t1, t2) -> Some (subs t2 n t1) 
  | Bop (App, Uop (Abs, t1), t2) when is_value t2 -> Some (subs t1 0 t2)
  | Bop (App, t1,       t2) when not (is_value t1) -> 
      Option.bind (red_step t1) (fun t -> Some (Bop (App, t, t2)))
  | Bop (App, t1,       t2) when not (is_value t2) ->
      Option.bind (red_step t2) (fun t -> Some (Bop (App, t1, t)))
  (* | Lis _        -> failwith "TODO reduction list" *)
  | _               -> None


(* Retourne the normal form of a term *)
let rec norm_rec n t =
  if n > 5000 then None else
  (* Option.fold ~none:(Some t) ~some:(norm_rec (n+1)) (red_step t)) *)
    (* fOption (ltr_cbv_norm_rec (n + 1)) t (ltr_cbv_step t) *)
    match red_step t with
    | Some t' -> (Pp.term Format.std_formatter (Option.get (red_step t));
                 (Common.Pp.nl Format.std_formatter ());
        norm_rec (n + 1) t')
    | None    -> Some t

let norm = norm_rec 0


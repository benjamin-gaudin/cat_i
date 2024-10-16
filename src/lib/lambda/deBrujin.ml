(* open Common.Ultils *)

type pterm = Nat of int
           | Var of int
           | Abs of pterm
           | App of pterm * pterm
           | Add of pterm * pterm
           | Mul of pterm * pterm


let rec string_of_term t =
  match t with
  | Nat x        -> string_of_int x
  | Var x        -> "ᵢ" ^ string_of_int x
  | Abs t        -> "(λ " ^ ( string_of_term t) ^ ")"
  | App (t1, t2) -> (string_of_term t1) ^ " " ^ (string_of_term t2) 
  | Add (t1, t2) -> (string_of_term t1) ^ " + " ^ (string_of_term t2) 
  | Mul (t1, t2) -> (string_of_term t1) ^ " * " ^ (string_of_term t2) 


(* lift index of free variables by one in a term*)
let rec lift_rec depth t =
  match t with
  | Nat x        -> Nat x
  | Var x        -> if x <= depth then t else Var (x + 1)
  | Abs t'       -> Abs (lift_rec (depth + 1) t')
  | App (t1, t2) -> App ((lift_rec depth t1), (lift_rec depth t2))
  | Add (t1, t2) -> Add ((lift_rec depth t1), (lift_rec depth t2))
  | Mul (t1, t2) -> Mul ((lift_rec depth t1), (lift_rec depth t2))

let lift = lift_rec 0


(* Substitute variables at index 0 by term u *)
let rec subs_rec d t u =
  match t with
  | Nat x        -> Nat x
  | Var x        -> if x = d then u else t
  | Abs t'       -> Abs (subs_rec (d + 1) t' (lift u))
  | App (t1, t2) -> App ((subs_rec d t1 u), (subs_rec d t2 u))
  | Add (t1, t2) -> Add ((subs_rec d t1 u), (subs_rec d t2 u))
  | Mul (t1, t2) -> Mul ((subs_rec d t1 u), (subs_rec d t2 u))

let subs = subs_rec 0

(* Return if a term is a value aka a non-reductible term *)
let is_value t =
  match t with
   | Nat _ | Var _ | Abs _ | App ((Var _), _) -> true
   | Add _ | Mul _ | App _                    -> false


(* Return a term after one step of beta reduction *)
let rec ltr_cbv_step t =
  match t with
  | Add (Nat 0, Nat n) | Add (Nat n, Nat 0)  ->
      Some (Nat n)
  | Mul (Nat 0, Nat _) | Mul (Nat _, Nat 0) -> Some (Nat 0)
  | Add (Nat m, Nat n)  -> Some (Nat (m + n))
  | Add (t1, t2) when not (is_value t1) ->
      Option.bind (ltr_cbv_step t1) (fun t -> Some (Add (t, t2)))
  | Add (t1, t2) when not (is_value t2) ->
      Option.bind (ltr_cbv_step t2) (fun t -> Some (Add (t1, t)))
  | Mul (Nat m, Nat n) -> Some (Add (Nat n, Mul (Nat (m - 1), Nat n)))
  | Mul (t1, t2) when not (is_value t1) ->
      Option.bind (ltr_cbv_step t1) (fun t -> Some (Mul (t, t2)))
  | Mul (t1, t2) when not (is_value t2) ->
      Option.bind (ltr_cbv_step t2) (fun t -> Some (Mul (t1, t)))
  | App ((Abs t1), t2) when is_value t2 -> 
      Some (subs t1 t2)
  | App (t1,       t2) when not (is_value t1) -> 
      Option.bind (ltr_cbv_step t1) (fun t -> Some (App (t, t2))) 
     (* (match ltr_cbv_step t1 with
      | Some t1' -> Some (App (t1', t2))
      | None     -> None) *)
  | App (t1,       t2) when not (is_value t2) ->
      Option.bind (ltr_cbv_step t2) (fun t -> Some (App (t1, t))) 
  | _               -> None

(* Retourne the normal form of a term *)
let rec ltr_cbv_norm_rec n t =
  if n > 5000 then None else (* TODO option *)
    (* fOption (ltr_cbv_norm_rec (n + 1)) t (ltr_cbv_step t) *)
    match ltr_cbv_step t with
    | Some t' -> ltr_cbv_norm_rec (n + 1) t'
    | None    -> Some t

let ltr_cbv_norm = ltr_cbv_norm_rec 0


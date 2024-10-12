type pterm = Var of int
           | Abs of pterm
           | App of pterm * pterm


let rec string_of_term t =
  match t with
  | Var x        -> string_of_int x
  | Abs t        -> "(λ " ^ ( string_of_term t) ^ ")"
  | App (t1, t2) -> "" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ "" 


(* lift index of free variables by one in a term*)
let rec lift_rec depth t =
  match t with
  | Var x        -> if x <= depth then t else Var (x + 1)
  | Abs t'       -> Abs (lift_rec (depth + 1) t')
  | App (t1, t2) -> App ((lift_rec depth t1), (lift_rec depth t2))

let lift = lift_rec 0


(* Substitute variables at index 0 by term u *)
let rec subs_rec d t u  =
  match t with
  | Var x        -> if x = d then u else t
  | Abs t'       -> Abs (subs_rec (d + 1) t' (lift u))
  | App (t1, t2) -> App ((subs_rec d t1 u), (subs_rec d t2 u))

let subs = subs_rec 0

(* Return if a term is a value aka a non-reductible term *)
let is_value t =
  match t with
   | Var _ | Abs _ | App ((Var _), _) -> true
   | _ -> false


(* Return a term after one step of beta reduction *)
let rec ltr_cbv_step t =
  match t with
  | App ((Abs t1), t2) when is_value t2 -> 
      Some (subs t1 t2)
  | App (t1,       t2) when not (is_value t1) -> 
     (match ltr_cbv_step t1 with
      | Some t1' -> Some (App (t1', t2))
      | None     -> None)
  | App (t1,       t2) when not (is_value t2) ->
     (match ltr_cbv_step t2 with
      | Some t2' -> Some (App (t1, t2'))
      | None     -> None)
  | _               -> None

(* Retourne the normal form of a term *)
let rec ltr_cbv_norm_rec n t =
  if n > 500 then failwith "divergent" else
    match ltr_cbv_step t with
    | Some t' -> ltr_cbv_norm_rec (n + 1) t'
    | None    -> t

let ltr_cbv_norm = ltr_cbv_norm_rec 0


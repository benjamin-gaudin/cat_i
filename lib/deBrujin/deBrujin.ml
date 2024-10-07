(* Definition Lambda terms purs *)
type pterm = Var of int
           | Abs of pterm
           | App of pterm * pterm


(* pretty printer *)
let rec string_of_term (t : pterm) : string =
  match t with
  | Var x        -> string_of_int x
  | Abs t        -> "λ " ^ ( string_of_term t)
  | App (t1, t2) -> "(" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ ")" 


(* lift pour incrémenter de 1 les indice de deBrujin, utile pour subs*)
let rec lift_rec (depth : int) (t : pterm) : pterm =
  match t with
  | Var x        -> if x <= depth then t else Var (x + 1)
  | Abs t'       -> Abs (lift_rec (depth + 1) t')
  | App (t1, t2) -> App ((lift_rec depth t1), (lift_rec depth t2))

let lift : pterm -> pterm = lift_rec 0


(* Substitution des variables libre x par le terme u *)
let rec subs_rec (d : int) (t : pterm) (x : int) (u : pterm) : pterm =
  match t with
  | Var y        -> if y = d + x then u else t
  | Abs t'       -> Abs (subs_rec (d + 1) t' x (lift u))
  | App (t1, t2) -> App ((subs_rec d t1 x u), (subs_rec d t2 x u))

let subs : pterm -> int -> pterm -> pterm = subs_rec 0

(* Retourne si un terme est une valeur. Une valeur est un terme non réduction *)
let is_value (t : pterm) : bool =
  match t with
   | Var _ | Abs _ | App ((Var _), _) -> true
   | _ -> false


(* Retourne un terme apres une beta reduction, si aucune reduction possible
   retourne le même terme *)
let rec ltr_cbv_step (t : pterm) : pterm =
  match t with
  | App ((Abs t1), t2) -> subs t1 0 t2
  | App (t1,       t2) -> if is_value t1 then App (t1, ltr_cbv_step t2)
                          else App (ltr_cbv_step t1, t2)
  | _                  -> t

(* Retourne la forme normale d'un terme *)
let rec ltr_cbv_norm_rec (n : int) (t : pterm) : pterm =
  if n > 1000 then failwith "divergent" else
      let t' = ltr_cbv_step t in
      if t' = t then t' else
          ltr_cbv_norm_rec (n + 1) t'

let ltr_cbv_norm : pterm -> pterm = ltr_cbv_norm_rec 0


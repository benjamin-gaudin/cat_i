(* open Common.Ultils *)
type plist = Nil
           | Con of pterm * plist

and pterm =  Nat of int
           | Var of int
           | Lis of plist
           | Fix of pterm
           | HD  of pterm
           | TL  of pterm
           | Abs of pterm
           | App of pterm * pterm
           | Add of pterm * pterm
           | Sub of pterm * pterm
           | Mul of pterm * pterm
           | Let of pterm * pterm * pterm
           | Ifz of pterm * pterm * pterm
           | Ifn of pterm * pterm * pterm

let rec plist_of_list l =
  match l with
  | [] -> Nil
  | t :: ts -> Con (t, (plist_of_list ts))

let rec list_of_plist l =
  match l with
  | Nil -> []
  | Con (t, ts) -> t :: (list_of_plist ts)

let rec string_of_term t =
  match t with
  | Nat x        -> string_of_int x
  | Var x        -> "ᵢ" ^ string_of_int x
  | Lis l        -> "[" ^ (List.fold_left 
    (fun acc t -> acc ^ string_of_term t ^ ";" ) "" (list_of_plist l)) ^ "]"
  | Fix t        -> "fix (" ^ string_of_term t ^ ")"
  | HD  t        -> "HD (" ^ string_of_term t ^ ")"
  | TL  t        -> "TL (" ^ string_of_term t ^ ")"
  | Abs t        -> "(λ " ^ ( string_of_term t) ^ ")"
  | App (t1, t2) -> (string_of_term t1) ^ " " ^ (string_of_term t2)
  | Add (t1, t2) -> (string_of_term t1) ^ " + " ^ (string_of_term t2)
  | Sub (t1, t2) -> (string_of_term t1) ^ " - " ^ (string_of_term t2)
  | Mul (t1, t2) -> (string_of_term t1) ^ " * " ^ (string_of_term t2)
  | Let (x, t1, t2) -> "let " ^ (string_of_term x) ^ " = " ^ 
      (string_of_term t1) ^ " in " ^ (string_of_term t2)
  | Ifz (c, t1, t2) -> "ifz " ^ (string_of_term c) ^ " then " ^
    (string_of_term t1) ^ " else " ^ (string_of_term t2)
  | Ifn (c, t1, t2) -> "ifn " ^ (string_of_term c) ^ " then " ^
    (string_of_term t1) ^ " else " ^ (string_of_term t2)


let rec lift_list_rec depth l =
  match l with
  | Nil         -> Nil
  | Con (t, ts) -> Con (lift_rec depth t, lift_list_rec depth ts)

(* lift index of free variables by one in a term*)
and lift_rec depth t =
  match t with
  | Nat x        -> Nat x
  | Var x        -> if x <= depth then t else Var (x + 1)
  | Lis l        -> Lis (lift_list_rec depth l)
  | Fix t        -> Fix (lift_rec depth t)
  | HD  t        -> HD  (lift_rec depth t)
  | TL  t        -> TL  (lift_rec depth t)
  | Abs t'       -> Abs (lift_rec (depth + 1) t')
  | App (t1, t2) -> App ((lift_rec depth t1), (lift_rec depth t2))
  | Add (t1, t2) -> Add ((lift_rec depth t1), (lift_rec depth t2))
  | Sub (t1, t2) -> Sub ((lift_rec depth t1), (lift_rec depth t2))
  | Mul (t1, t2) -> Mul ((lift_rec depth t1), (lift_rec depth t2))
  | Let (x, t1, t2) -> 
      Let ((lift_rec depth x), (lift_rec depth t1), (lift_rec depth t2))
  | Ifz (c, t1, t2) -> 
      Ifz ((lift_rec depth c), (lift_rec depth t1),(lift_rec depth t2))
  | Ifn (c, t1, t2) -> 
      Ifn ((lift_rec depth c), (lift_rec depth t1),(lift_rec depth t2))

let lift = lift_rec 0

let rec subs_list_rec d l v u =
  match l with
  | Nil         -> Nil
  | Con (t, ts) -> Con (subs_rec d t v u, subs_list_rec d ts v u)

(* Substitute variables at index 0 by term u *)
and subs_rec d t v u =
  match t with
  | Nat x        -> Nat x
  | Var x        -> if x = d + v then u else t
  | Lis l        -> Lis (subs_list_rec d l v u)
  | Fix t        -> Fix (subs_rec d t v u)
  | HD  t        -> HD  (subs_rec d t v u)
  | TL  t        -> TL  (subs_rec d t v u)
  | Abs t'       -> Abs (subs_rec (d + 1) t' v (lift u))
  | App (t1, t2) -> App (subs_rec d t1 v u, subs_rec d t2 v u)
  | Add (t1, t2) -> Add (subs_rec d t1 v u, subs_rec d t2 v u)
  | Sub (t1, t2) -> Sub (subs_rec d t1 v u, subs_rec d t2 v u)
  | Mul (t1, t2) -> Mul (subs_rec d t1 v u, subs_rec d t2 v u)
  | Let (x, t1, t2) -> 
      Let (subs_rec d x v u, subs_rec d t1 v u, subs_rec d t2 v u)
  | Ifz (c, t1, t2) -> 
      Ifz (subs_rec d c v u, subs_rec d t1 v u, subs_rec d t2 v u)
  | Ifn (c, t1, t2) -> 
      Ifn (subs_rec d c v u, subs_rec d t1 v u, subs_rec d t2 v u)

let subs = subs_rec 0

(* Return if a term is a value aka a non-reductible term *)
let is_value t =
  match t with
   | Nat _ | Var _ | Abs _ | App ((Var _), _) | Lis _ -> true
   | Add _ | Sub _ | Mul _ | App _ | HD _ | TL _ | Ifz _ |
     Ifn _ | Fix _ | Let _ -> false


(* Return a term after one step of beta reduction *)
let rec ltr_cbv_step t =
  match t with
  | HD  (Lis Con (t, _)) -> Some t
  | TL  (Lis Con (_, ts)) -> Some (Lis ts)
  | Add (Nat 0, Nat n) | Add (Nat n, Nat 0) | Sub (Nat 0, Nat n) 
    | Sub (Nat n, Nat 0) -> Some (Nat n)
  | Mul (Nat 0, Nat _) | Mul (Nat _, Nat 0) -> Some (Nat 0)
  | Add (Nat m, Nat n) -> Some (Nat (m + n))
  | Sub (Nat m, Nat n) -> Some (Nat (m - n))
  | Add (t1, t2) when not (is_value t1) ->
      Option.bind (ltr_cbv_step t1) (fun t -> Some (Add (t, t2)))
  | Add (t1, t2) when not (is_value t2) ->
      Option.bind (ltr_cbv_step t2) (fun t -> Some (Add (t1, t)))
  | Sub (t1, t2) when not (is_value t1) ->
      Option.bind (ltr_cbv_step t1) (fun t -> Some (Sub (t, t2)))
  | Sub (t1, t2) when not (is_value t2) ->
      Option.bind (ltr_cbv_step t2) (fun t -> Some (Sub (t1, t)))
  | Mul (Nat m, Nat n) -> Some (Add (Nat n, Mul (Nat (m - 1), Nat n)))
  | Mul (t1, t2) when not (is_value t1) ->
      Option.bind (ltr_cbv_step t1) (fun t -> Some (Mul (t, t2)))
  | Mul (t1, t2) when not (is_value t2) ->
      Option.bind (ltr_cbv_step t2) (fun t -> Some (Mul (t1, t)))
  | Ifz (c, t1, t2) when not (is_value c) ->
      Option.bind (ltr_cbv_step c) (fun t -> Some (Ifz (t, t1, t2)))
  | Ifz (Nat 0, t1, _) -> Some t1
  | Ifz (_,     _, t2) -> Some t2
  | Ifn (c, t1, t2) when not (is_value c) ->
      Option.bind (ltr_cbv_step c) (fun t -> Some (Ifn (t, t1, t2)))
  | Ifn (Lis Nil, t1, _) -> Some t1
  | Ifn (_,       _, t2) -> Some t2
  | Fix (Abs t1      ) -> Some (subs t1 0 (Fix (Abs t1)))
  | Let (x, t1, t2) when not (is_value t1) -> 
      Option.bind (ltr_cbv_step t1) (fun t -> Some (Let (x, t, t2)))
  | Let (Var x, t1, t2) -> Some (subs t2 x t1) (* TODO choose depth in subs*)
  | App (Abs t1,   t2) when is_value t2 -> Some (subs t1 0 t2)
  | App (t1,       t2) when not (is_value t1) -> 
      Option.bind (ltr_cbv_step t1) (fun t -> Some (App (t, t2))) 
  | App (t1,       t2) when not (is_value t2) ->
      Option.bind (ltr_cbv_step t2) (fun t -> Some (App (t1, t))) 
  (* | Lis _        -> failwith "TODO reduction list" *)
  | _               -> None

(* Retourne the normal form of a term *)
let rec ltr_cbv_norm_rec n t =
  if n > 5000 then None else
    (* fOption (ltr_cbv_norm_rec (n + 1)) t (ltr_cbv_step t) *)
    match ltr_cbv_step t with
    | Some t' -> ltr_cbv_norm_rec (n + 1) t'
    | None    -> Some t

let ltr_cbv_norm = ltr_cbv_norm_rec 0


type ptype = Nat
           | Var of string
           | Lis of ptype
           | Gen of ptype * ptype
           | Arr of ptype * ptype

(* Pretty printer *)
let rec string_of_ptype ty =
  match ty with
  | Nat            -> "Nat"
  | Var x          -> x
  | Lis l          -> "[" ^ string_of_ptype l ^ "]"
  | Gen (x, ty)     -> "∀ " ^ string_of_ptype x ^ ". " ^ string_of_ptype ty
  | Arr (ty1, ty2) ->
      "(" ^ (string_of_ptype ty1) ^ " -> " ^ (string_of_ptype ty2) ^ ")"

(* Create a type with a new label *)
let new_ptype =
  let cpt = ref (-1) in
  fun () -> incr cpt; Var ("T" ^ ( string_of_int !cpt ))

type equa = (ptype * ptype) list

(* Pretty printer *)
let string_of_equa eq =
  List.fold_left (fun x y -> x ^ y) ""
  (List.map (fun x -> "\n" ^ string_of_ptype (fst x) ^ " = " ^ 
                            string_of_ptype (snd x)) eq)

(* Environement type *)
type env = (int * ptype) list

open DeBrujin
type ptype = Var of string
           | Arr of ptype * ptype

(* Pretty printer *)
let rec string_of_ptype ty =
  match ty with
  | Var x          -> x
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
  (List.map (fun x -> "(" ^ string_of_ptype (fst x) ^ "," ^ 
                            string_of_ptype (snd x) ^ ")") eq)

(* Environement type *)
(* type env = (int * ptype) list *)

(* Get the type of the variables v in the environement e *)
let get_type v e =
  snd (List.find (fun x -> fst x = v) e)

exception FreeVariableType of (int * ptype)

(* Create the equations systeme for the type of a terms *)
let rec gen_equa_rec depth e (t : pterm) ty =
  match t with
  | Var v        ->
      (try [(get_type (depth - 1 - v) e), ty] with (* TODO catch Not_found *) (* avoir un type pour les termes libres *)
      | Not_found -> raise (FreeVariableType (depth - 1 - v, new_ptype ())))
  | Abs t'       -> let ta = new_ptype() in
                    let tr = new_ptype() in
                      (ty, Arr (ta, tr)) :: 
                        (gen_equa_rec (depth + 1) ((depth, ta)::e) t' tr )
  | App (t1, t2) -> let ta = new_ptype() in
                    try
                      (gen_equa_rec depth e t1 (Arr (ta, ty)))
                        @ (gen_equa_rec depth e t2 ta)
                    with FreeVariableType (v, ty') ->
                        (* gen_equa_rec depth ((v,ty') :: e) t ty *)
                      (gen_equa_rec depth ((v, ty') :: e) t1 (Arr (ta, ty)))
                        @ (gen_equa_rec depth ((v, ty') :: e) t2 ta)

let gen_equa : pterm -> ptype -> equa = gen_equa_rec 0 []











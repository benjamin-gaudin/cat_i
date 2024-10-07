open DeBrujin

(* Type pour les terms de lambda calcul pur *)
type ptype = Var of string
           | Arr of ptype * ptype

(* Pretty printer *)
let rec string_of_ptype (t : ptype) : string =
  match t with
  | Var x        -> x
  | Arr (t1, t2) -> "(" ^ (string_of_ptype t1) ^ " -> " ^ (string_of_ptype t2) ^ ")"

(* Genere des nouveaux labels pour les types *)
let new_ptype : unit -> ptype =
  let cpt = ref (-1) in
  fun () -> incr cpt; Var ("T" ^ ( string_of_int !cpt ))

(* let new_ptype : unit -> ptype =
  let cpt = ref (-1) in
  fun () -> incr cpt;
  let label = "T" ^ ( string_of_int !cpt ) in
  let label = new_label_type() in
  fun () -> Var label *)

  (* Var (new_label_type ()) *)

(* Type des équation, une liste de types à unifer *)
type equa = (ptype * ptype) list

let string_of_equa (eq : equa) : string =
  List.fold_left (fun x y -> x ^ y) ""
  (List.map (fun x -> "(" ^ string_of_ptype (fst x) ^ "," ^ 
                            string_of_ptype (snd x) ^ ")") eq)

(* Type des environement *)
type env = (int * ptype) list

(* Recupere le type d'une varible v dans l'environement e *)
let get_type (v : int) (e : env) : ptype =
  snd (List.find (fun x -> fst x = v) e)

(* Genere les equations sur les types des terms d'un terme *)
let rec gen_equa_rec (depth : int) (e : env) (t : pterm) (ty : ptype) : equa =
  match t with
  | Var v        -> [(get_type (depth - 1 - v) e), ty] (* TODO catch Not_found *)
  | Abs t'       -> let ta = new_ptype() in
                    let tr = new_ptype() in
                    (ty, Arr (ta, tr)) :: 
                      (gen_equa_rec (depth + 1) ((depth, ta)::e) t' tr )
  | App (t1, t2) -> let ta = new_ptype() in
                    (gen_equa_rec depth e t1 (Arr (ta, ty)))
                      @ (gen_equa_rec depth e t2 ta)
let gen_equa : pterm -> ptype -> equa = gen_equa_rec 0 []











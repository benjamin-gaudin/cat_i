open DeBrujin
type ptype = Nat
           | Var of string
           | Arr of ptype * ptype

(* Pretty printer *)
let rec string_of_ptype ty =
  match ty with
  | Nat            -> "Nat"
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
  (List.map (fun x -> "\n" ^ string_of_ptype (fst x) ^ " = " ^ 
                            string_of_ptype (snd x)) eq)

(* Environement type *)
type env = (int * ptype) list

(* Get the type of the variables v in the environement e *)
let get_type v e =
  snd (List.find (fun x -> fst x = v) e)

exception FVExp of (int * ptype)

(* Create the equations system for the type of a terms *)
let rec gen_eq_r ?(fv=true) d e (t : pterm) ty : (ptype * ptype) list * env =
  match t with
  | Nat _        -> ([ty, Nat], [])
  | Var v        ->
      (try ([(get_type (d - 1 - v) e), ty], []) with
      | Not_found ->
        if fv then raise (FVExp (d - 1 - v, new_ptype ()))
        else ([], []))
  | Abs t'       ->
      let ta = new_ptype() in
      let tr = new_ptype() in
      let (eqs, fvs) = (gen_eq_r ~fv:fv (d + 1) ((d, ta)::e) t' tr) in
      ((ty, Arr (ta, tr)) :: eqs, fvs)
  | App (t1, t2) ->
      let ta = new_ptype() in
      (try
        let (eqs1, fvs1) = (gen_eq_r ~fv:fv d e t1 (Arr (ta, ty))) in
        let (eqs2, fvs2) = (gen_eq_r ~fv:fv d e t2 ta) in
        (eqs1 @ eqs2, fvs1 @ fvs2)
      with FVExp e_fv -> (fun (vars, fvs) -> (vars, e_fv :: fvs) )
        (gen_eq_r ~fv:fv d (e_fv::e) t ty)
      )


  | Add (t1, t2) | Mul (t1, t2) ->
      let call = fun e e_fv ->
        let (eqs1, fvs1) = gen_eq_r ~fv:fv d e t1 Nat in
        let (eqs2, fvs2) = gen_eq_r ~fv:fv d e t2 Nat in
        (((ty, Nat) :: eqs1 @ eqs2), e_fv @ fvs1 @ fvs2)
      in
      (try call e [] with FVExp e_fv -> call (e_fv :: e) [e_fv])

let gen_equa ?(fv=true) t ty = try gen_eq_r ~fv:fv 0 [] t ty with
               | FVExp (var, ty) -> print_endline "not catched";
                   ([Var "goal", ty], [var,ty])




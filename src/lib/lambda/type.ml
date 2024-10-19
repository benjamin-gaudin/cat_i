open DeBrujin
type ptype = Nat
           | Var of string
           | Lis of ptype
           | Arr of ptype * ptype

(* Pretty printer *)
let rec string_of_ptype ty =
  match ty with
  | Nat            -> "Nat"
  | Var x          -> x
  | Lis l          -> "[" ^ string_of_ptype l ^ "]"
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

exception FVExp of (int * ptype)

(* Create the equations system for the type of a terms *)
let rec gen_eq_r ?(fv=true) d e (t : pterm) ty : (ptype * ptype) list * env =
  match t with
  | Nat _        -> ([ty, Nat], [])
  | Var v        ->
      (try ([(List.assoc (d - 1 - v) e), ty], []) with
      | Not_found ->
        if fv then raise (FVExp (d - 1 - v, new_ptype ()))
        else ([], []))
  | Lis Nil      -> ([ty, Lis (new_ptype ())], [])
  | Lis (Con (t, ts)) ->
      let ta = new_ptype() in
      (try
        let (eqs1, fvs1) = (gen_eq_r ~fv:fv d e t ta) in
        let (eqs2, fvs2) = (gen_eq_r ~fv:fv d e (Lis ts) (Lis ta)) in
        ((ty, Lis ta) :: eqs1 @ eqs2, fvs1 @ fvs2)
      with FVExp e_fv -> (fun (vars, fvs) -> (vars, e_fv :: fvs))
        (gen_eq_r ~fv:fv d (e_fv::e) t ty)
      )
  | HD t         -> 
      let ta = new_ptype() in
      let (eqs, fvs) = gen_eq_r ~fv:fv d e t (Lis ta) in
      ((ty, ta) :: eqs, fvs)
  | TL t         -> 
      let ta = new_ptype() in
      let (eqs, fvs) = gen_eq_r ~fv:fv d e t ta in
      ((ty, ta) :: eqs, fvs)
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
      (try
        let (eqs1, fvs1) = gen_eq_r ~fv:fv d e t1 Nat in
        let (eqs2, fvs2) = gen_eq_r ~fv:fv d e t2 Nat in
        (((ty, Nat) :: eqs1 @ eqs2), fvs1 @ fvs2)
      with FVExp e_fv -> (fun (vars, fvs) -> (vars, e_fv :: fvs) )
                (gen_eq_r ~fv:fv d (e_fv::e) t ty))
  | Ifz (c, t1, t2) ->
      let ta = new_ptype() in
      (try
      let (eqsc, fvsc) = (gen_eq_r ~fv:fv d e c Nat) in
      let (eqs1, fvs1) = (gen_eq_r ~fv:fv d e t1 ta) in
      let (eqs2, fvs2) = (gen_eq_r ~fv:fv d e t2 ta) in
      ((ty, ta) :: eqsc @ eqs1 @ eqs2, fvsc @ fvs1 @ fvs2)
      with FVExp e_fv -> (fun (vars, fvs) -> (vars, e_fv :: fvs))
        (gen_eq_r ~fv:fv d (e_fv::e) t ty)
      )
  | Ifn (c, t1, t2) ->
      let ta = new_ptype() in
      let tr = new_ptype() in
      (try
      let (eqsc, fvsc) = (gen_eq_r ~fv:fv d e c (Lis ta)) in
      let (eqs1, fvs1) = (gen_eq_r ~fv:fv d e t1 tr) in
      let (eqs2, fvs2) = (gen_eq_r ~fv:fv d e t2 tr) in
      ((ty, tr) :: eqsc @ eqs1 @ eqs2, fvsc @ fvs1 @ fvs2)
      with FVExp e_fv -> (fun (vars, fvs) -> (vars, e_fv :: fvs))
        (gen_eq_r ~fv:fv d (e_fv::e) t ty)
      )


let gen_equa ?(fv=true) t ty = try gen_eq_r ~fv:fv 0 [] t ty with
               | FVExp (var, ty) -> print_endline "not catched";
                   ([Var "goal", ty], [var,ty])




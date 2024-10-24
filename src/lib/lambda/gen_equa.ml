open Common.Type
open Common.Ast
open Error

let rec vars_of_type ty =
  match ty with
  | Common.Type.Nat | Gen _ -> []
  | Var x -> [Common.Type.Var x]
  | Lis ty ->  vars_of_type ty
  | Arr (ty1, ty2) -> (vars_of_type ty1) @ (vars_of_type ty2)

(* Create the equations system for the type of a terms *)
let rec gen_eq_r d e (t : term) ty : (ptype * ptype) list =
  (* print_endline ("start ------ ");
  Pp.term Format.std_formatter t; Common.Pp.nl Format.std_formatter ();
  Pp.env Format.std_formatter e; Common.Pp.nl Format.std_formatter ();
  print_endline ("end ------ "); *)
  match t with
  | Nil   -> [ty, Lis (new_ptype ())]
  | Nat _ -> [ty, Nat]
  | Var v -> (
              (* print_endline ("d = " ^ (string_of_int (d - 1 - v)));
              print_endline ("env ------ ");
              Pp.env Format.std_formatter e;
              print_endline ("------ "); *)
              try [ty, (List.assoc (d - 1 - v) e)] with
              | Not_found -> raise (FVNotFound t))
              (* | Not_found -> []) *)
  | Con (t, ts)         -> gen_eq_r_Con d e t ts ty
  | Uop (u, t)          -> gen_eq_r_Uop d e u t ty
  | Bop (b, t1, t2)     -> gen_eq_r_Bop d e b t1 t2 ty
  | Let (x, t1, t2)     -> gen_eq_r_Let d e x t1 t2 ty
  | Cod (co, c, t1, t2) -> gen_eq_r_Cod d e co c t1 t2 ty

and gen_eq_r_Con d e t ts ty : (ptype * ptype) list =
      let ta = new_ptype() in
      let eqs1 = gen_eq_r d e t ta in
      let eqs2 = gen_eq_r d e ts (Lis ta) in
      (ty, Common.Type.Lis ta) :: eqs1 @ eqs2

and gen_eq_r_Uop d e u t ty =
  match (u, t) with
  | (HD, t) ->
      let ta = new_ptype() in
      let eqs = gen_eq_r d e t (Lis ta) in
      (ty, ta) :: eqs
  | (TL, t) ->
      let ta = new_ptype() in
      let eqs = gen_eq_r d e t ta in
      (ty, ta) :: eqs
  | (Abs, t') ->
      let ta = new_ptype() in
      let tr = new_ptype() in
      let eqs = gen_eq_r (d + 1) ((d, ta) :: e) t' tr in
      (ty, Arr (ta, tr)) :: eqs
  | (Fix, t) ->
      let ta = new_ptype() in
      let tr = new_ptype() in
      let t' = (match t with
                | Uop (Abs, t') -> t'
                | _             -> raise (UntypeableFix (Uop (Fix, t))))
      in
      let eqs = gen_eq_r d ((d - 1, Arr (ta, tr)) :: e) t' (Arr (ta, tr)) in
      (ty, Arr(ta,tr)) :: eqs

and gen_eq_r_Bop d e b t1 t2 ty =
  match (b, t1, t2) with
  | (App, t1, t2) ->
      let ta = new_ptype() in
      let eqs1 = gen_eq_r d e t1 (Arr (ta, ty)) in
      let eqs2 = gen_eq_r d e t2 ta in
      eqs1 @ eqs2
  | (Add, t1, t2) | (Mul, t1, t2) | (Sub, t1, t2) ->
      let eqs1 = gen_eq_r d e t1 Nat in
      let eqs2 = gen_eq_r d e t2 Nat in
      ((ty, Common.Type.Nat) :: eqs1 @ eqs2)

and gen_eq_r_Let d e x t1 t2 ty =
    let ta = new_ptype() in
    let eq = gen_eq_r d e t1 ta in 
    let eq_ta = Unification.resolve eq ta in
    match x, eq_ta with
    | Var x, Some ((_, ta) :: _ ) ->
        let fvs = vars_of_type ta in
        let ta = List.fold_left (fun acc ty -> (Gen(ty, acc))) ta fvs in
        let eqs = gen_eq_r d ((- x -1, ta) :: e) t2 ty in
        eqs
    | _                   -> raise (UntypeableLet (Let (x, t1, t2)))

and gen_eq_r_Cod d e co c t1 t2 ty =
  match co, c, t1, t2 with
  | (Ifz, c, t1, t2) ->
      let ta = new_ptype() in
      let eqsc = gen_eq_r d e c Nat in
      let eqs1 = gen_eq_r d e t1 ta in
      let eqs2 = gen_eq_r d e t2 ta in
      (ty, ta) :: eqsc @ eqs1 @ eqs2
  | (Ifn, c, t1, t2) ->
      let ta = new_ptype() in
      let tr = new_ptype() in
      let eqsc = gen_eq_r d e c (Lis ta) in
      let eqs1 = gen_eq_r d e t1 tr in
      let eqs2 = gen_eq_r d e t2 tr in
      (ty, tr) :: eqsc @ eqs1 @ eqs2


let gen_equa t ty = try gen_eq_r 0 [] t ty with
               | FVNotFound (_) -> 
                   failwith ("TODO FVNotFound ")
               | UntypeableLet (_) -> 
                   failwith ("TODO UntypeableLet ")
               | UntypeableFix (_) -> 
                   failwith ("TODO UntypeableFix ")


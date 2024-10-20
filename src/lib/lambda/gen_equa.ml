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
let rec gen_eq_r  d e (t : term) ty : (ptype * ptype) list =
  match t with
  | Nat _        -> [ty, Nat]
  | Var v        ->
      (try [ty, (List.assoc (d - 1 - v) e)] with
      | Not_found -> raise (FVNotFound t))
  | Lis Nil      -> [ty, Lis (new_ptype ())]
  | Lis (Con (t, ts)) ->
      let ta = new_ptype() in
      let eqs1 = gen_eq_r d e t ta in
      let eqs2 = gen_eq_r d e (Lis ts) (Lis ta) in
      (ty, Common.Type.Lis ta) :: eqs1 @ eqs2
  | Uop (HD, t) -> 
      let ta = new_ptype() in
      let eqs = gen_eq_r d e t (Lis ta) in
      (ty, ta) :: eqs
  | Uop (TL, t) -> 
      let ta = new_ptype() in
      let eqs = gen_eq_r d e t ta in
      (ty, ta) :: eqs
  | Uop (Abs, t')       ->
      let ta = new_ptype() in
      let tr = new_ptype() in
      let eqs = gen_eq_r (d + 1) ((d, ta) :: e) t' tr in
      (ty, Arr (ta, tr)) :: eqs
  | Bop (App, t1, t2) ->
      let ta = new_ptype() in
      let eqs1 = gen_eq_r d e t1 (Arr (ta, ty)) in
      let eqs2 = gen_eq_r d e t2 ta in
      eqs1 @ eqs2
  | Bop (Add, t1, t2) | Bop (Mul, t1, t2) | Bop (Sub, t1, t2) ->
      let eqs1 = gen_eq_r d e t1 Nat in
      let eqs2 = gen_eq_r d e t2 Nat in
      ((ty, Common.Type.Nat) :: eqs1 @ eqs2)
  | Let (x, t1, t2) ->
      let ta = new_ptype() in
      let eq = gen_eq_r d e t1 ta in 
      let eq_ta = Unification.resolve eq ta in
      (match x, eq_ta with
      | Var x, Some ((_, ta) :: _ ) ->
          let fvs = vars_of_type ta in
          let ta = List.fold_left (fun acc ty -> (Gen(ty, acc))) ta fvs in
          let eqs = gen_eq_r d ((- x -1, ta) :: e) t2 ty in
          eqs
      | _                   -> raise (UntypeableLet t))
  | Cod (Ifz, c, t1, t2) ->
      let ta = new_ptype() in
      let eqsc = gen_eq_r d e c Nat in
      let eqs1 = gen_eq_r d e t1 ta in
      let eqs2 = gen_eq_r d e t2 ta in
      (ty, ta) :: eqsc @ eqs1 @ eqs2
  | Cod (Ifn, c, t1, t2) ->
      let ta = new_ptype() in
      let tr = new_ptype() in
      let eqsc = gen_eq_r d e c (Lis ta) in
      let eqs1 = gen_eq_r d e t1 tr in
      let eqs2 = gen_eq_r d e t2 tr in
      (ty, tr) :: eqsc @ eqs1 @ eqs2
  | Uop (Fix, t) ->
      let ta = new_ptype() in
      let tr = new_ptype() in
      let t' = (match t with
                | Uop (Abs, t') -> t'
                | _             -> raise (UntypeableFix t))
      in
      let eqs = gen_eq_r d ((d, Arr (ta, tr)) :: e) t' (Arr (ta, tr)) in
      (ty, Arr(ta,tr)) :: eqs

let gen_equa t ty = try gen_eq_r 0 [] t ty with
               | FVNotFound (_) -> 
                   failwith ("TODO FVNotFound ")
               | UntypeableLet (_) -> 
                   failwith ("TODO UntypeableLet ")
               | UntypeableFix (_) -> 
                   failwith ("TODO UntypeableFix ")


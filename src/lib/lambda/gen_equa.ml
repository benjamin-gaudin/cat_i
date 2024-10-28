open Common.Type
open Common.Ast
open Error

let rec concat_uniq l1 l2 =
  match l1 with
  | [] -> l2
  | x :: xs when List.mem x l2 -> concat_uniq xs l2
  | x :: xs ->  x :: concat_uniq xs l2

let rec vars_of_type (ty : ptype) =
  match ty with
  | Bol | Nat | Gen _ -> []
  | Var x             -> [Common.Type.Var x]
  | Lis ty            ->  vars_of_type ty
  | Arr (ty1, ty2)    -> concat_uniq (vars_of_type ty1) (vars_of_type ty2)


(* Create the equations system for the type of a terms *)
let rec gen_eq_r d e (t : term) ty : (ptype * ptype) list =
  match t with
  | Cst Tru | Cst Fal   -> [ty, Bol]
  | Cst Nil             -> [ty, Lis (new_ptype ())]
  | Nat _               -> [ty, Nat]
  | Var v               -> (try [ty, (List.assoc (d - 1 - v) e)] with
                            | Not_found -> eraise (FVNotFound t))
  | Uop (u, t)          -> gen_eq_r_Uop d e u t ty
  | Bop (b, t1, t2)     -> gen_eq_r_Bop d e b t1 t2 ty
  | Let (x, t1, t2)     -> gen_eq_r_Let d e x t1 t2 ty
  | Cod (co, c, t1, t2) -> gen_eq_r_Cod d e co c t1 t2 ty

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
                | _             -> eraise (UntypeableFix (Uop (Fix, t))))
      in
      let eqs = gen_eq_r d ((d - 1, Arr (ta, tr)) :: e) t' (Arr (ta, tr)) in
      (ty, Arr(ta,tr)) :: eqs

and gen_eq_r_Bop d e b t1 t2 ty =
  match (b, t1, t2) with
  | (And, t1, t2) | (Or, t1, t2) ->
      let eqs1 = gen_eq_r d e t1 Bol in
      let eqs2 = gen_eq_r d e t2 Bol in
      ((ty, Common.Type.Bol) :: eqs1 @ eqs2)
  | (Con, t, ts) ->
      let ta = new_ptype() in
      let eqs1 = gen_eq_r d e t ta in
      let eqs2 = gen_eq_r d e ts (Lis ta) in
      (ty, Common.Type.Lis ta) :: eqs1 @ eqs2
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
    | _                   -> eraise (UntypeableLet (Let (x, t1, t2)))

and gen_eq_r_Cod d e co c t1 t2 ty =
  match co, c, t1, t2 with
  | (If, c, t1, t2) ->
      let ta = new_ptype() in
      let eqsc = gen_eq_r d e c Bol in
      let eqs1 = gen_eq_r d e t1 ta in
      let eqs2 = gen_eq_r d e t2 ta in
      (ty, ta) :: eqsc @ eqs1 @ eqs2
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
               | E e -> Pp.err Format.std_formatter e; []


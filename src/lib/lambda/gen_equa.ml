open Common.Utils
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
  | Lis ty            -> vars_of_type ty
  | Rcd tys           ->
      List.fold_left
        (fun acc (_, ty) -> concat_uniq (vars_of_type ty) acc) [] tys
  | Vrt tys           ->
      List.fold_left
        (fun acc (_, ty) -> concat_uniq (vars_of_type ty) acc) [] tys
  | Arr (ty1, ty2)    -> concat_uniq (vars_of_type ty1) (vars_of_type ty2)

let rec rm_gen (ty : ptype) =
  match ty with
  | Bol | Nat         -> ty
  | Gen (_, tys)      -> rm_gen tys
  | Var x             -> Var x
  | Lis ty            -> rm_gen ty
  | Rcd tys           -> Rcd (List.map (fun (l, ty) -> (l, (rm_gen ty))) tys)
  | Vrt tys           -> Rcd (List.map (fun (l, ty) -> (l, (rm_gen ty))) tys) (* OK ? *)
  | Arr (ty1, ty2)    -> Arr (rm_gen ty1, rm_gen ty2)


(* Create the equations system for the type of a terms *)
let rec gen_eq_r d e (t : term) ty : (ptype * ptype) list =
  match t with
  | Cst Tru | Cst Fal   -> [ty, Bol]
  | Cst Nil             -> [ty, Lis (new_ptype ())]
  | Nat _               -> [ty, Nat]
  | Lbl _               -> failwith "ERROR : Typing a label"
  | Var v               -> (try [ty, (List.assoc (d - 1 - v) e)] with
                            | Not_found -> eraise (FVNotFound t))
  | Rcd ts              -> gen_eq_r_Rcd d e ts ty
  | Vrt ts              -> gen_eq_r_Vrt d e ts ty 
  | Cas (v, ts)         -> gen_eq_r_Cas d e v ts ty
  | As  (_, typ)        -> [(ty, typ)]
  | Uop (u, t)          -> gen_eq_r_Uop d e u t ty
  | Bop (b, t1, t2)     -> gen_eq_r_Bop d e b t1 t2 ty
  | Let (x, t1, t2)     -> gen_eq_r_Let d e x t1 t2 ty
  | Cod (co, c, t1, t2) -> gen_eq_r_Cod d e co c t1 t2 ty

and get_type d e t err =
  let ta = new_ptype() in
  let eq = gen_eq_r d e t ta in 
  let vrt_t0 = Unification.resolve eq ta in
  match vrt_t0  with
    | Some ty_t0 -> snd (List.hd ty_t0)
    | None -> eraise err

and gen_eq_r_Rcd d e ts ty =
  let new_vars = List.map (fun _ -> new_ptype()) ts in
  let lbls = assoc_keys ts in
  if not (l_no_duplicate lbls) then 
    failwith "TODO labels of the records are not unique" else
  let fields = zip lbls new_vars in
  let eqs = List.fold_left2
    (fun acc (_,t) i -> (gen_eq_r d e t i) @ acc) [] ts new_vars in
  (ty, Rcd fields) :: eqs

and gen_eq_r_Vrt d e ts ty =
  let new_vars = List.map (fun _ -> new_ptype()) ts in
  let lbls = assoc_keys ts in
  if not (l_no_duplicate lbls) then 
    failwith "TODO labels of the variant are not unique" else
  let vrt = zip lbls new_vars in
  let eqs = List.fold_left2
    (fun acc var (_,t) -> gen_eq_r d e t var @ acc) [] new_vars ts in
  (ty, Vrt vrt) :: eqs

and gen_eq_r_Cas d e v ts ty =
  let vrt_t0 = get_type d e v (PrjNotNat v) in
  match vrt_t0 with
  | Vrt vrt_t0 -> 
      let list_typs =
      List.fold_left2
        (fun acc (l_t0, ty_t0) (l, t) ->
         if not (l_t0 = l) then failwith "TODO Case not same type" else
         let ta = new_ptype() in
         let eq = gen_eq_r (d + 1) ((d, ty_t0) :: e) t ta in 
         (ta, eq) :: acc
        ) [] vrt_t0 ts
      in
      (* all branches and return type have same type @ equations for all branches *)
        list_to_l_pair_trans (ty :: assoc_keys (list_typs)) @
        List.fold_left (fun acc (_,y) -> y @ acc) [] list_typs
  | _       -> failwith "TODO case on type different from variant"


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
      (ty, Common.Type.Bol) :: eqs1 @ eqs2
  | (Prj, t, ts) ->
    (match t, ts with
      | Nat n, Rcd ts ->
        if n >= List.length ts then eraise (PrjOutOfBound (Rcd ts)) else
        let new_vars = List.map (fun _ -> new_ptype()) ts in
        let eqs = List.fold_left2
          (fun acc (_, t) i -> (gen_eq_r d e t i) @ acc) [] ts new_vars in
        (ty, List.nth new_vars n) :: eqs
      | Lbl s, Rcd ts ->
        let new_vars = List.map (fun _ -> new_ptype()) ts in
        let lbls = assoc_keys ts in (* TODO projection with label not found*)
        let eqs = List.fold_left2
          (fun acc (_,t) i -> (gen_eq_r d e t i) @ acc) [] ts new_vars in
        let fields = zip lbls new_vars in
        (ty, List.assoc s fields) :: eqs
      | Nat n, Var x ->
          let record_in_e = rm_gen (List.assoc (d - 1 - x) e) in
          (match record_in_e with
          | Rcd ty_record  ->
              let ty_x = snd (List.nth ty_record n) in
              let fvs = vars_of_type ty_x in
              let ty_x_gen = List.fold_left
                (fun acc ty -> (Gen(ty, acc))) ty_x fvs in
              [ty, ty_x_gen]
          | _ -> failwith "TODO projection on variable which is not a record")
      | Lbl s, Var x ->
          let record_in_e = (try rm_gen (List.assoc (d - 1 - x) e) with
                             | Not_found -> failwith "TODO prj on variable in environement not found")
              in
          (match record_in_e with
          | Rcd ty_record  -> 
              let ty_x = (try List.assoc s ty_record with
                         | Not_found -> failwith "TODO prj on lbl in variable not found")
              in
              let fvs = vars_of_type ty_x in
              let ty_x_gen = List.fold_left
                (fun acc ty -> (Gen(ty, acc))) ty_x fvs in
              [ty, ty_x_gen]
          | _ -> failwith "TODO projection on variable which is not a record")
      | _ -> eraise (PrjNotNat t)) (* TODO or label and record*)
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
      (ty, Common.Type.Nat) :: eqs1 @ eqs2

and gen_eq_r_Let d e x t1 t2 ty =
    let ta = new_ptype() in
    let eq = gen_eq_r d e t1 ta in 
    let eq_ta = Unification.resolve eq ta in
    match x, eq_ta with
    | Var x, Some ((_, ta) :: _ ) ->
        let fvs = vars_of_type ta in
        let ta = List.fold_left (fun acc ty -> (Gen(ty, acc))) ta fvs in
        let eqs = gen_eq_r d ((d -1 - x, ta) :: e) t2 ty in
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


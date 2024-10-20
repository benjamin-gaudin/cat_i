open Common.Type
open DeBrujin

type equa = (ptype * ptype) list

(* Pretty printer *)
let string_of_equa eq =
  List.fold_left (fun x y -> x ^ y) ""
  (List.map (fun x -> "\n" ^ string_of_ptype (fst x) ^ " = " ^ 
                            string_of_ptype (snd x)) eq)

let rec concat_uniq xs ys =
  match xs with
  | [] -> ys
  | x :: xs -> if List.mem x xs then concat_uniq xs ys 
               else concat_uniq xs (x :: ys)

let rec vars_of_type ty =
  match ty with
  | Common.Type.Nat | Gen _ -> []
  | Var x -> [Common.Type.Var x]
  | Lis ty ->  vars_of_type ty
  | Arr (ty1, ty2) -> concat_uniq (vars_of_type ty1) (vars_of_type ty2)

(* Environement type *)
type env = (int * ptype) list

exception UntypeableLet of pterm
exception UntypeableFix of pterm
exception FVExp of (int * ptype)

(* Create the equations system for the type of a terms *)
let rec gen_eq_r ?(fv=true) d e (t : pterm) ty : (ptype * ptype) list * env =
  match t with
  | Nat _        -> ([ty, Nat], [])
  | Var v        ->
      (try ([ty, (List.assoc (d - 1 - v) e)], []) with
      | Not_found ->
        (* print_endline "Not_found variable in env"; *)
        (* print_endline ((string_of_int (d - 1 - v)) ^ " vs " ^ (string_of_int (fst (List.hd e)))); *)
        (* print_endline (List.fold_left (fun acc (var, ty)-> "\n    " ^ DeBrujin.string_of_term (Var (- var)) ^ " : " ^ string_of_ptype ty ^ acc) "" e); *)
        if fv then raise (FVExp (d - 1 - v, new_ptype ()))
        else ([], []))
  | Lis Nil      -> ([ty, Lis (new_ptype ())], [])
  | Lis (Con (t, ts)) ->
      let ta = new_ptype() in
      (try
        let (eqs1, fvs1) = gen_eq_r ~fv:fv d e t ta in
        let (eqs2, fvs2) = gen_eq_r ~fv:fv d e (Lis ts) (Lis ta) in
        ((ty, Common.Type.Lis ta) :: eqs1 @ eqs2, fvs1 @ fvs2)
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
      let (eqs, fvs) = gen_eq_r ~fv:fv (d + 1) ((d, ta)::e) t' tr in
      ((ty, Arr (ta, tr)) :: eqs, fvs)
  | App (t1, t2) ->
      let ta = new_ptype() in
      (try
        let (eqs1, fvs1) = gen_eq_r ~fv:fv d e t1 (Arr (ta, ty)) in
        let (eqs2, fvs2) = gen_eq_r ~fv:fv d e t2 ta in
        (eqs1 @ eqs2, fvs1 @ fvs2)
      with FVExp e_fv -> (fun (vars, fvs) -> (vars, e_fv :: fvs) )
        (gen_eq_r ~fv:fv d (e_fv::e) t ty)
      )
  | Add (t1, t2) | Mul (t1, t2) | Sub (t1, t2) ->
      (try
        let (eqs1, fvs1) = gen_eq_r ~fv:fv d e t1 Nat in
        let (eqs2, fvs2) = gen_eq_r ~fv:fv d e t2 Nat in
        (((ty, Common.Type.Nat) :: eqs1 @ eqs2), fvs1 @ fvs2)
      with FVExp e_fv -> (fun (vars, fvs) -> (vars, e_fv :: fvs) )
                (gen_eq_r ~fv:fv d (e_fv::e) t ty))
  | Let (x, t1, t2) ->
      let ta = new_ptype() in
      (* let tr = new_ptype() in *)
      (* let ta = Unification.ptype_of_term ~fv:false t1 in *)
      let (eq, _) = gen_eq_r ~fv:true d e t1 ta in 
      let eq_ta = Unification.resolve eq ta in
      (* print_endline ("Equa of N with ta = " ^ (string_of_ptype ta) );
      print_endline (Common.Type.string_of_equa eq);
      print_endline "after resolving";
      print_endline (Common.Type.string_of_equa (Option.get eq_ta)); *)
      (match x, eq_ta with
      | Var x, Some ((_, ta) :: _ ) ->
          (* print_endline ("ta : " ^ string_of_ptype ta); *)
          let fvs = vars_of_type ta in
          let ta = List.fold_left (fun acc ty -> (Gen(ty, acc))) ta fvs in
          (* print_endline ("new env for l let in : " ^ string_of_ptype ta); *)
          let (eqs, fvs) = gen_eq_r ~fv:fv d ((- x -1, ta) :: e) t2 ty in
          (* ((tr, ta) :: eqs, fvs) *)
          (eqs, fvs)
      | _                   -> raise (UntypeableLet t))
  | Ifz (c, t1, t2) ->
      let ta = new_ptype() in
      (try
      let (eqsc, fvsc) = gen_eq_r ~fv:fv d e c Nat in
      let (eqs1, fvs1) = gen_eq_r ~fv:fv d e t1 ta in
      let (eqs2, fvs2) = gen_eq_r ~fv:fv d e t2 ta in
      ((ty, ta) :: eqsc @ eqs1 @ eqs2, fvsc @ fvs1 @ fvs2)
      with FVExp e_fv -> (fun (vars, fvs) -> (vars, e_fv :: fvs))
        (gen_eq_r ~fv:fv d (e_fv::e) t ty)
      )
  | Ifn (c, t1, t2) ->
      let ta = new_ptype() in
      let tr = new_ptype() in
      (try
      let (eqsc, fvsc) = gen_eq_r ~fv:fv d e c (Lis ta) in
      let (eqs1, fvs1) = gen_eq_r ~fv:fv d e t1 tr in
      let (eqs2, fvs2) = gen_eq_r ~fv:fv d e t2 tr in
      ((ty, tr) :: eqsc @ eqs1 @ eqs2, fvsc @ fvs1 @ fvs2)
      with FVExp e_fv -> (fun (vars, fvs) -> (vars, e_fv :: fvs))
        (gen_eq_r ~fv:fv d (e_fv::e) t ty)
      )
  | Fix (t) ->
      let ta = new_ptype() in
      let tr = new_ptype() in
      let t' =
      (match t with
      | Abs t' -> t'
      | _      -> raise (UntypeableFix t))
      in
      (try
      let (eqs, fvs) = gen_eq_r ~fv:fv d ((d, Arr (ta, tr)) :: e) t' (Arr (ta, tr)) in
      (* let (eqs, fvs) = gen_eq_r ~fv:fv d ((d, Arr (ta, tr)) :: e) t tr in *)
      ((ty, Arr(ta,tr)) :: eqs, fvs)
      with FVExp e_fv -> (fun (vars, fvs) -> (vars, e_fv :: fvs))
        (gen_eq_r ~fv:fv d (e_fv::e) t ty)
      )




let gen_equa ?(fv=true) t ty = try gen_eq_r ~fv:fv 0 [] t ty with
               | UntypeableLet (t) -> 
                   failwith ("TODO UntypeableLet : " ^ string_of_term t)
               | UntypeableFix (t) -> 
                   failwith ("TODO UntypeableFix : " ^ string_of_term t)
               | FVExp (var, ty) -> print_endline "not catched";
                   ([Var "goal", ty], [var,ty])




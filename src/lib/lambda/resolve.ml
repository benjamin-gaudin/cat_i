open Gen_equa
open Unification

(* Infere type of a term *)
let ptype_of_term ?(fv=true) t =
  let (eqs, fvs) = gen_equa ~fv:fv t (Var "goal") in
  let req_Goal = resolve eqs (Var "goal") in
  let (fvs_var, fvs_ty) = List.split fvs in
  let reqs_fv = List.map (resolve eqs) fvs_ty in
  if List.exists Option.is_none reqs_fv then None else
  let fvs_rty = List.map (fun x -> snd (List.hd (Option.get x))) reqs_fv in
  let env = List.combine fvs_var fvs_rty in
  match req_Goal with
  | None | Some []  -> None
  | Some req        -> Some ((snd (List.hd req)), env)

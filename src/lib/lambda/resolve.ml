open Gen_equa
open Unification

(* Infere type of a term *)
let ptype_of_term t =
  let eqs = gen_equa t (Var "goal") in
  let req_Goal = resolve eqs (Var "goal") in
  match req_Goal with
  | None | Some [] -> None
  | Some req       -> Some (snd (List.hd req))

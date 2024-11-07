type cst = Nat | Bol | Uni

type ptype = Cst of cst
           | Var of string
           | Lis of ptype
           | Rcd of (string * ptype) list
           | Vrt of (string * ptype) list
           | Gen of ptype * ptype
           | Arr of ptype * ptype

(* Create a type with a new label *)
let new_ptype =
  let cpt = ref (-1) in
  fun () -> incr cpt; Var ("T" ^ ( string_of_int !cpt ))

type equa = (ptype * ptype) list


open DeBrujin

type ptype = Nat
           | Var of string
           | Arr of ptype * ptype

(* Pretty printer ptype *)
val string_of_ptype : ptype -> string

type equa = (ptype * ptype) list

(* Pretty printer *)
val string_of_equa : equa -> string

type env = (int * ptype) list

(* Create the equations systeme for the type of a terms *)
val gen_equa : ?fv:bool -> pterm -> ptype -> equa * env


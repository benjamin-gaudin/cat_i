open Common.Type
open DeBrujin

(* Définition des systèmes d'équations *)
type equa = (ptype * ptype) list

(* Pretty printer des systèmes d'équations *)
val string_of_equa : equa -> string

(* Genere les equations sur les types des terms d'un terme *)
val gen_equa : ?fv:bool -> pterm -> ptype -> (equa * env)

open Type
open DeBrujin

(* Resolve a equations systeme for a "goal" *)
val resolve : equa -> equa option

(* Infere type of a term *)
val ptype_of_term : pterm -> ptype option

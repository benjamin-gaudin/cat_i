open Common.Type
(* open DeBrujin *)

(* Resolve a equations systeme for a "goal" *)
val resolve : equa -> ptype -> equa option

(* Infere type of a term *)
(* val ptype_of_term : ?fv:bool -> pterm -> (ptype * env) option *)

open Common.Type
open Common.Ast

(* Genere les equations sur les types des terms d'un terme *)
val gen_equa : term -> ptype -> equa 

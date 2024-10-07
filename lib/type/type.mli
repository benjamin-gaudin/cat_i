open DeBrujin

(* Definition des types pour les terms de lambda calcul pur *)
type ptype = Var of string
           | Arr of ptype * ptype

(* Pretty printer ptype *)
val string_of_ptype : ptype -> string

(* Définition des systèmes d'équations *)
type equa = (ptype * ptype) list

(* Pretty printer des systèmes d'équations *)
val string_of_equa : equa -> string

(* Genere les equations sur les types des terms d'un terme *)
val gen_equa : pterm -> ptype -> equa


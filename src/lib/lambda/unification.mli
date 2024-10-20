open Common.Type

(* 'resolve eq ty' resolve an equations system eq for a goal ty *)
val resolve : equa -> ptype -> equa option

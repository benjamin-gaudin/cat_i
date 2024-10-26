type opt = Eq

let eq = ref false


let set = function
  | Eq -> eq := true

let set_list l =
  List.iter set l

let reset = 
  eq := false


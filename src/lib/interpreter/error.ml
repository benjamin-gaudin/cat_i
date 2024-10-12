open Format

type t =
  (* Frontend --------------------------------------------------------------- *)
  | Input_not_found of string
  | Input_dir       of string
  | Parsing_error   of string
  | Lexing_error    of string

exception E of t

let pp fmt = function
  | Input_not_found f ->
      fprintf fmt "Input '%s' does not exists" f
  | Input_dir f ->
      fprintf fmt "Input '%s' is a directory" f
  | Parsing_error s ->
      fprintf fmt "Found unexpected '%s'" (String.escaped s)
  | Lexing_error s ->
      fprintf fmt "Unknown character '%s'" (String.escaped s)

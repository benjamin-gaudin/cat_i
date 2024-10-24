type t =
  | EInput          of string
  | EParse          of string
  | Lexing_error    of string

exception E of t

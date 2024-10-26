type t =
  | EInput  of string
  | EParse  of string
  | ELexing of string

exception E of t

let eraise e =
    raise (E e)

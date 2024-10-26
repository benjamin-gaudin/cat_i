open Common.Ast

type t =
  | UntypeableLet of term
  | UntypeableFix of term
  | FVNotFound    of term

exception E of t

let eraise e =
    raise (E e)

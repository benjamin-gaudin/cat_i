open Common.Ast

type t =
  | EUntypeableLet    of term
  | EUntypeableFix    of term
  | EFVNotFound       of term
  | EPrjOutOfBound    of int * term
  | EPrjLabelNotFound of string * term
  | EPrjNotVrt        of term
  | EPrjNotNatOrLbl   of term * term
  | ELabelRcdUniq     of term
  | ELabelVrtUniq     of term
  | ECaseNotVrt       of term
  | ECaseNotSameVrt   of term * term

exception E of t

let eraise e =
    raise (E e)

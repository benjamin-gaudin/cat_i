open Format
open Ast

let cond fmt = function
  | Ifz -> fprintf fmt "Ifz"
  | Ifn -> fprintf fmt "Ifn"

let uop fmt = function
  | Fix -> fprintf fmt "fix"
  | HD  -> fprintf fmt "hd"
  | TL  -> fprintf fmt "tl"
  | Abs -> fprintf fmt "λ"

let bop fmt = function
  | App -> fprintf fmt "@"
  | Add -> fprintf fmt "+"
  | Sub -> fprintf fmt "-"
  | Mul -> fprintf fmt "*"

let nl fmt _ =
  fprintf fmt "\n"

let semi fmt _ =
  fprintf fmt ";"

let tab fmt _ =
  fprintf fmt "\t"

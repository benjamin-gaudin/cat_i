open Format
open Error
open Ast

let const fmt = function
  | Nil -> fprintf fmt "[]"
  | Tru -> fprintf fmt "true"
  | Fal -> fprintf fmt "false"

let cond fmt = function
  | If  -> fprintf fmt "If"
  | Ifz -> fprintf fmt "Ifz"
  | Ifn -> fprintf fmt "Ifn"

let uop fmt = function
  | Fix -> fprintf fmt "fix"
  | HD  -> fprintf fmt "hd"
  | TL  -> fprintf fmt "tl"
  | Abs -> fprintf fmt "λ"

let bop fmt = function
  | And -> fprintf fmt "&&"
  | Or  -> fprintf fmt "||"
  | App -> fprintf fmt ""
  | Add -> fprintf fmt "+"
  | Sub -> fprintf fmt "-"
  | Mul -> fprintf fmt "*"
  | Con -> fprintf fmt "::"
  | Prj -> fprintf fmt "prj" (* TODO *)

let nl fmt _ =
  fprintf fmt "\n"

let semi fmt _ =
  fprintf fmt ";"

let colon fmt _ =
  fprintf fmt ","

let tab fmt _ =
  fprintf fmt "\t"

let err fmt = function
  | EInput f ->
      fprintf fmt "couldn't open input file@;%s" f
  | EParse s ->
      fprintf fmt "Found unexpected '%s'" (String.escaped s)
  | ELexing s ->
      fprintf fmt "Unknown character '%s'" (String.escaped s)

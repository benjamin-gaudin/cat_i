open Common.Type
open Common
open Format
open Lambda
open Sys

let nl = Common.Pp.nl std_formatter
let print_term = Pp.term  std_formatter
let print_type = Pp.ttype std_formatter
let print_equa = Pp.equas std_formatter

let err pp e =
    eprintf "@[<hov 4>@{<fg_red>@{<bold>error(%s):@}@}@;%a@]@." pp e

let help () =
  eprintf "usage:\n";
  eprintf "    cat_i <command> <arguments>\n";
  eprintf "commands:\n";
  eprintf "    exec\n";
  eprintf "    help\n"

let from_file (f : string) =
  let c = 
      try open_in f
      with _ -> Common.Error.eraise (Common.Error.EInput f)
  in
  let lb  = Lexing.from_channel c in
  let p =
      try Interpreter.Parser.program Interpreter.Lexer.token lb 
      with Interpreter.Parser.Error -> 
        Common.Error.eraise (Common.Error.EParse (Lexing.lexeme lb))
  in
  p

let print_equa t =
    try
      (printf "Equa        : "; nl ();
      (print_equa (Gen_equa.gen_equa t (Var "goal"))); nl ();)
    with Lambda.Error.E e -> Lambda.Pp.err std_formatter e

let print_nf t =
  let t' = Reduction.norm t in
  printf "Normal Form : ";
  match t' with
  | None    -> printf "Divergent (Timeout)"
  | Some t' -> print_term t'

let print_type t =
  try
    match Resolve.ptype_of_term t with
    | None    -> printf "Type        : Untypeable@."
    | Some ty -> printf "Type        : "; print_type ty; nl ()
  with Lambda.Error.E e -> Lambda.Pp.err std_formatter e
  

let test (t, options) =
  Options.set_list options;
  printf "----------------@." ;
  printf "Term        : ";
  print_term t; nl ();
  print_nf t; nl ();
  if !Options.eq then print_equa t else ();
  print_type t

let exec file =
  eprintf "interpreting %s@." file;
  try
    let prog =  from_file file in
    List.iter test prog
  with Common.Error.E e -> err file Common.Pp.err e

let () =
    Colorsh.setup_std ();
    if Array.length argv < 2 then help () else
    let input = Array.sub argv 2 (Array.length argv - 2) in
    match argv.(1) with
    | "exec" -> Array.iter exec input
    | _      -> help ()


open Common.Ultils
open Common.Type
open Format
open Lambda
open Sys

let nl = Common.Pp.nl std_formatter
let print_term = Pp.term  std_formatter
let print_type = Pp.ttype std_formatter
let print_equa = Pp.equas std_formatter


let help () =
  eprintf "usage:\n";
  eprintf "    lambda <command> <arguments>\n";
  eprintf "commands:\n";
  eprintf "    exec\n";
  eprintf "    help\n"

let from_file (f : string) =
  if not (Sys.file_exists f) then raise (Interpreter.Error.E (Input_not_found f));
  if (Sys.is_directory f) then raise (Interpreter.Error.E (Input_dir f));
  let c   = open_in f             in
  let lb  = Lexing.from_channel c in
  let p = Interpreter.Parser.program Interpreter.Lexer.token lb in
  p

let test (t, options) =
  printf "----------------@." ;
  printf "Term        : ";
  print_term t; nl ();
  let t' = Reduction.norm t in
  printf "Normal Form : ";
  match t' with
  | None    -> printf "Divergent (Timeout)"
  | Some t' -> print_term t'; 
  nl ();
  if List.mem Eq options then (printf "Equa        : "; 
    (print_equa (Gen_equa.gen_equa t (Var "goal"))))
  else ();
  match Resolve.ptype_of_term t with
  | None    -> printf "Type        : Untypeable@."
  | Some ty -> (printf "Type        : "; print_type ty; nl ();)

let exec file =
    eprintf "interpreting %s@." file;
    let prog = from_file file in
    List.iter test prog

let () =
  if Array.length argv < 2 then help () else
  let input = Array.sub argv 2 (Array.length argv - 2) in
  match argv.(1) with
  | "exec" -> Array.iter exec input
  | _      -> help ()


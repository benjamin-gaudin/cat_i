open Format
open Lambda
open Sys

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
  let p =
    (* try   *)
      Interpreter.Parser.program Interpreter.Lexer.token lb
    (* with Parser.Error -> raise (E (Parsing_error (Lexing.lexeme lb))) *)
  in
  (* ignore (Typer.t_prog p); *)
  p

let test t =
  printf "----------------@." ;
  printf "Term        : %s@." (DeBrujin.string_of_term t);
  let t' = DeBrujin.ltr_cbv_norm t in
  printf "Normal Form : %s@."
    (Common.Ultils.fOption DeBrujin.string_of_term "Divergent (Timeout)" t');
  printf "Equa        : %s@." (Type.string_of_equa (Type.gen_equa t (Type.Var "goal")));
  (* printf "Equa NF     : %s@." (Type.string_of_equa (Type.gen_equa t' (Type.Var "goal"))); *)
  printf "Type        : %s@."
    (Common.Ultils.fOption Type.string_of_ptype "Untypeable" (Unification.ptype_of_term t))
  (* printf "Type NF     : %s@."
    (Common.Ultils.fOption Type.string_of_ptype "Untypeable" (Unification.ptype_of_term t')) *)
  (* let sty = 
    match Unification.ptype_of_term t with
    | Some ty -> Type.string_of_ptype ty
    | None    -> "Non typable"
  in
  printf "Type        : %s@." sty *)


let exec file =
  (* try *)
    eprintf "interpreting %s@." file;
    let prog  = from_file file     in
    (* let value = Interpreter.i_prog prog in *)
    (* printf "%s@." (Pp.value value) *)
    List.iter test prog
  (* with
  | Failure e        -> eprintf "failure: %s@." e
  | Tinyml.Error.E e -> eprintf "%a %a@." error () Tinyml.Error.pp e
  | Common.Error.E e -> eprintf "%a %a@." error () Common.Error.pp e *)

let () =
  if Array.length argv < 2 then help () else
  let input = Array.sub argv 2 (Array.length argv - 2) in
  match argv.(1) with
  | "exec" -> Array.iter exec input
  | _      -> help ()


(* λ 5 1 *)
(* let t0 = Abs (App (Var 4, Var 0)) *)
(* let t0 = Abs (Var 0) *)
(* let t0' = lift t0 *)
(* let t0'' = gen_equa t0 (Var "but") *)
(* let () = print_endline (print_equa t0'') *)

(* let () =
if Var "T1" = Var "T1" then
  print_endline "true"
else
  print_endline "false" *)


(* let t3 = Abs (Abs (Var 1)) *)
(* let t3' = gen_equa t3 (Var "but") *)
(* let t3'' = resolve t3' *)

(* let () = print_endline "t3'"
let () = print_endline (print_equa t3')
let () = print_endline "t3''"
let () = print_endline (print_equa t3'') *)
(* let () = print_endline (print_term t0') *)

(* (λ λ 4 2 (λ 1 3)) (λ 5 1) *)
(* let t1 : pterm = App (Abs (Abs (App (Var 3, App (Var 1, Abs (App (Var 0, Var 2)))))), Abs (App (Var 4, Var 0)))
let () = print_endline (string_of_term t1)
let () = print_endline (string_of_equa (gen_equa t1 (Var "but")))
let () = print_endline (string_of_ptype (ptype_of_term t1)) *)

(* λ 3 (λ 6 1) (λ 1 (λ 7 1)) *)
(* λ 4 (λ 6 2) (λ 1 (λ 7 3)) *)
(* let t1' = ltr_cbv_step t1 *)
(* let t1'' = ltr_cbv_norm t1 *)

(* let t2 = App (App (Abs (Abs (Var 1)), Abs (Var 0)), Abs (Var 0))
let t2' = ltr_cbv_norm t2
let t2'' = gen_equa t2 (Var "but")
(* let t2''' = resolve t2'' *)
let () = print_endline (string_of_ptype (ptype_of_term t2)) *)

(* λx λy λz (x z) (y z) *)
(* λ  λ  λ  (2 0) (1 0) *)
(* let s = Abs (Abs (Abs (App (App (Var 2, Var 0), App(Var 1, Var 0)))))
let () = print_endline (string_of_term s)
(* let s' = gen_equa s (Var "but") *)
(* let s'' = resolve s' *)
let () = print_endline (string_of_ptype (ptype_of_term s)) *)

(* let () = print_endline (print_term t2) *)
(* let () = print_endline (print_term t2') *)
(* let () = print_endline (print_equa t2'') *)
(* let () = print_endline (print_equa t2''') *)

(* let () = print_endline (print_term t1)
let () = print_endline (print_term t1') *)
(* let () = print_endline (print_term t1'') *)

(* λ  λ  (2 0)  λ (2 1) *)
(* let t4 = Abs (Abs (App (App (Var 2, Var 0), Abs (App(Var 2, Var 1)))))
let () = print_endline (string_of_term t4)
let () = print_endline (string_of_ptype (ptype_of_term t4)) *)

(* let t1 = Abs (Abs (App (Var 1, Var 0))) *)
(* let () = print_endline (string_of_equa (gen_equa t1 (Var "but"))) *)
(* let () = print_endline (string_of_ptype (ptype_of_term t1)) *)
(* let tadd =  *)
  (* Abs (Abs (Abs (Abs (App (App (Var 3, Var 1), (App (App(Var 2, Var 1), Var 0))))))) *)
(* let t1_add_1 : pterm = App (App (tadd, t1), t1) *)
(* let tadd_1 : pterm = App ((tadd), (t1)) *)
(* let () = print_endline ("add 1 : " ^ (string_of_term tadd_1)) *)
(* let t1_add_1' = ltr_cbv_norm t1_add_1 *)
(* let () = print_endline (string_of_term t1_add_1) *)
(* let () = print_endline (string_of_term t1_add_1') *)
(* let () = print_endline (string_of_equa (gen_equa t1_add_1' (Var "but"))) *)
(* let () = print_endline (string_of_ptype (ptype_of_term t1_add_1')) *)

(* let () = print_endline "λ ello, World!" *)
(* add 1 (λ λ λ λ ((3 1) ((2 1) 0)) λ λ (1 0) ) *)

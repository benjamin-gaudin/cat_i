open LambdaOcaml.DeBrujin
open LambdaOcaml.Type
open LambdaOcaml.Unification

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
(* let t1 : pterm = App (Abs (Abs (App (Var 3, App (Var 1, Abs (App (Var 0, Var 2)))))), Abs (App (Var 4, Var 0))) *)

(* λ 3 (λ 6 1) (λ 1 (λ 7 1)) *)
(* λ 4 (λ 6 2) (λ 1 (λ 7 3)) *)
(* let t1' = ltr_cbv_step t1 *)
(* let t1'' = ltr_cbv_norm t1 *)

(* let t2 = App (App (Abs (Abs (Var 1)), Abs (Var 0)), Abs (Var 0))
let t2' = ltr_cbv_norm t2
let t2'' = gen_equa t2 (Var "but")
let t2''' = resolve t2'' *)

let s = Abs (Abs (Abs (App (App (Var 2, Var 0), App(Var 1, Var 0)))))
let s' = gen_equa s (Var "but")
let s'' = resolve s'

(* let () = print_endline (print_term t2) *)
(* let () = print_endline (print_term t2') *)
(* let () = print_endline (print_equa t2'') *)
(* let () = print_endline (print_equa t2''') *)

(* let () = print_endline (print_term t1)
let () = print_endline (print_term t1') *)
(* let () = print_endline (print_term t1'') *)


let () = print_endline "λ ello, World!"

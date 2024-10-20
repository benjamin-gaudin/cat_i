open Common.Ast

(* `subs t x u` Substitute variables at index x by term u in term t *)
val subs : term -> int -> term -> term

(* `norm t`give the normal form of then term t*)
val norm : term -> term option

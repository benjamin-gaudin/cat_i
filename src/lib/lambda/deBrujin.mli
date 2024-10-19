type plist = Nil
           | Con of pterm * plist

and pterm =  Nat of int
           | Var of int
           | Lis of plist
           | HD  of pterm
           | TL  of pterm
           | Abs of pterm
           | App of pterm * pterm
           | Add of pterm * pterm
           | Mul of pterm * pterm

val plist_of_list : pterm list -> plist

(* pretty printer *)
val string_of_term : pterm -> string

(* Substitute variables at index 0 by term u *)
val subs : pterm -> pterm -> pterm

(* Retourne the normal form of a term *)
val ltr_cbv_norm : pterm -> pterm option


(* Definition Lambda terms purs *)
type pterm = Var of int
           | Abs of pterm
           | App of pterm * pterm

(* pretty printer *)
val string_of_term : pterm -> string

(* lift pour incrémenter de 1 les indice des variables libes du terms*)
val lift : pterm -> pterm

(* Substitution des variables libre x par le terme u *)
val subs : pterm -> int -> pterm -> pterm

(* Retourne un terme apres une beta reduction, si aucune reduction possible
   retourne le même terme *)
val ltr_cbv_step : pterm -> pterm
(* Retourne la forme normale d'un terme *)
val ltr_cbv_norm : pterm -> pterm


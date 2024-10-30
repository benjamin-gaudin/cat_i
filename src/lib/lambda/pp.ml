open Common.Ultils
open Common.Type
open Common.Ast
open Common.Pp
open Format
open Error

let rec term fmt = function
  | Cst c         -> fprintf fmt "%a" const c
  | Nat n           -> fprintf fmt "%d" n
  | Var n           -> fprintf fmt "ᵢ%d" n
  | Bop (Con, t, ts)->
      fprintf fmt "[%a]" (pp_print_list ~pp_sep:semi term) (list_of_tlist (Bop (Con, t,ts)))
  | Uop (u, t)      -> fprintf fmt "%a %a" uop u term t
  | Bop (b, t1, t2) -> fprintf fmt "(%a %a %a)" term t1 bop b term t2
  | Let (x, t1, t2) -> fprintf fmt "let %a = %a in %a" term x term t1 term t2
  | Cod (co, c, t1, t2) -> 
      fprintf fmt "%a %a then %a else %a" cond co term c term t1 term t2

let rec ttype fmt (t : ptype) =
match t with
  | Nat            -> fprintf fmt "Nat"
  | Bol            -> fprintf fmt "Bool"
  | Var s          -> fprintf fmt "%s" s
  | Lis n          -> fprintf fmt "[%a]" ttype n
  | Gen (x, ty)    -> fprintf fmt "∀ %a. %a" ttype x ttype ty
  | Arr (ty1, ty2) -> fprintf fmt "(%a -> %a)" ttype ty1 ttype ty2
  | Pai (ty1, ty2) -> fprintf fmt "(%a * %a)" ttype ty1 ttype ty2

let equa fmt t = fprintf fmt "%a = %a" ttype (fst t) ttype (snd t)

let equas fmt t = fprintf fmt "%a" (pp_print_list ~pp_sep:nl equa) t

let env_aux fmt t = fprintf fmt "%d : %a" (fst t) ttype (snd t)

let env fmt t = fprintf fmt "%a" (pp_print_list ~pp_sep:nl env_aux) t

let err fmt = function
  | FVNotFound t ->
      fprintf fmt "The variable %a was not found" term t
  | UntypeableLet t ->
      fprintf fmt "The term %a in the let in can't be typed" term t
  | UntypeableFix t ->
      fprintf fmt "Then term %a in a fix can't be typed" term t

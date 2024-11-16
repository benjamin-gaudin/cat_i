open Common.Utils
open Common.Type
open Common.Ast
open Common.Pp
open Format
open Error

let rec ttype fmt (t : ptype) =
match t with
  | Cst Uni        -> fprintf fmt "unit"
  | Cst Nat        -> fprintf fmt "nat"
  | Cst Bol        -> fprintf fmt "bool"
  | Var s          -> fprintf fmt "%s" s
  | Lis n          -> fprintf fmt "[%a]" ttype n
  | Rcd tys        -> fprintf fmt "{@[<hv>%a@]}" (pp_print_list ~pp_sep:colon rcdfieldt) tys
  | Vrt tys        -> fprintf fmt "<@[<hv>%a@]>" (pp_print_list ~pp_sep:colon rcdfieldt) tys
  | Gen (x, ty)    -> fprintf fmt "∀ %a. %a" ttype x ttype ty
  | Arr (ty1, ty2) -> fprintf fmt "(%a -> %a)" ttype ty1 ttype ty2

and rcdfieldt fmt field = 
  let (l, t) =  field in
  if l = "" then fprintf fmt "%a" ttype t
  else fprintf fmt "%s : %a" l ttype t

let rec term fmt = function
  | Cst c           -> fprintf fmt "%a" const c
  | Nat n           -> fprintf fmt "%d" n
  | Lbl s           -> fprintf fmt "%s" s
  | Var n           -> fprintf fmt "ᵢ%d" n
  | Rcd ts          -> fprintf fmt "{@[<hv>%a@]}"
      (pp_print_list ~pp_sep:colon rcdfieldv) ts
  | Vrt ts          -> fprintf fmt "<@[<hv>%a@]>"
      (pp_print_list ~pp_sep:colon rcdfieldv) ts
  | Uop (u, t)      -> fprintf fmt "@[<hov 2>%a@ %a@]" uop u term t
  | As  (t, ty)     -> fprintf fmt "@[<hov 2>%a as@ %a@]" term t ttype ty
  | Bop (Prj, n, t) -> fprintf fmt "@[<hov 2>π %a@ %a@]" term n term t
  | Bop (Con, t, ts)->
      fprintf fmt "@,[@[<h>%a@]]" (pp_print_list ~pp_sep:semi term)
        (list_of_tlist (Bop (Con, t,ts)))
  | Bop (b, t1, t2) -> fprintf fmt "(%a %a %a)" term t1 bop b term t2
  | Let (x, t1, t2) -> fprintf fmt "@[<hv>let %a =@;<1 2>@[<hv 2>%a@]@ in@ @]%a"
      term x term t1 term t2
  | Cod (co, c, t1, t2) -> 
      fprintf fmt "@[<hv 2>%a %a then@ %a@ else %a@]"
        cond co term c term t1 term t2
  | Cas (t, ts) ->
      fprintf fmt "case %a of@;<1 2>@[<hv>%a@],"
      term t (pp_print_list caseField) ts

and rcdfieldv fmt field =
  let (l, t) =  field in
  if l = "" then fprintf fmt "%a" term t
  else fprintf fmt "%s = @[<hv 2>%a@]" l term t

and caseField fmt field =
  let (l, t) =  field in
  fprintf fmt "@[<hov 2><%s=i0> ->@;<1 2>%a@]" l term t

let equa fmt t = fprintf fmt "%a = %a" ttype (fst t) ttype (snd t)
let equas fmt t = fprintf fmt "@[<v 2>%a@]" (pp_print_list equa) t

let env_aux fmt t = fprintf fmt "%d : %a" (fst t) ttype (snd t)
let env fmt t = fprintf fmt "%a" (pp_print_list env_aux) t

let err fmt = function
  | FVNotFound t ->
      fprintf fmt "The variable %a was not found" term t
  | UntypeableLet t ->
      fprintf fmt "The term :@ %a@ in the let in can't be typed" term t
  | UntypeableFix t ->
      fprintf fmt "Then term %a in a fix can't be typed" term t
  | PrjOutOfBound t ->
      fprintf fmt "Projection too far '%a'" term t
  | PrjNotNat  t->
      fprintf fmt "Projection firt argument need to be a number '%a'" term t

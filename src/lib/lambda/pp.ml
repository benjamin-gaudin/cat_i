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

let ttype_rn fmt (t : ptype) = let t = rn_ty t in ttype fmt t

let rec term fmt = function
  | Cst c           -> fprintf fmt "%a" const c
  | Nat n           -> fprintf fmt "%d" n
  | Lbl s           -> fprintf fmt "%s" s
  | Var n           -> fprintf fmt "ᵢ%d" n
  | Rcd ts          -> fprintf fmt "{@[<hv>%a@]}"
      (pp_print_list ~pp_sep:colon rcdfieldv) ts
  | Vrt ts          -> fprintf fmt "<@[<hv>%a@]>"
      (pp_print_list ~pp_sep:colon rcdfieldv) ts
  | Uop (u, t)      -> fprintf fmt "%a@ %a" uop u term t
  | As  (t, ty)     -> fprintf fmt "%a as@ %a" term t ttype ty
  | Bop (Prj, n, t) -> fprintf fmt "π %a@ %a" term n term t
  | Bop (Con, t, ts)->
      fprintf fmt "@,[@[<h>%a@]]" (pp_print_list ~pp_sep:semi term)
        (list_of_tlist (Bop (Con, t,ts)))
  | Bop (b, t1, t2) -> fprintf fmt "(%a %a %a)" term t1 bop b term t2
  | Let (x, t1, t2) -> fprintf fmt "@[<hv>let %a =@;<1 2>@[<hov 2>%a@]@ in@ @]%a"
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
let equas_rn fmt t =
  let t = rn_eq t in
  fprintf fmt "@[<v 2>%a@]" (pp_print_list equa) t

let env_aux fmt t = fprintf fmt "%d : %a" (fst t) ttype (snd t)
let env fmt t = fprintf fmt "%a" (pp_print_list env_aux) t

let err fmt = function
  | EFVNotFound t ->
      fprintf fmt "The variable %a was not found" term t
  | EUntypeableLet t ->
      fprintf fmt "@[<v 2>The term in the let in can't be typed :@ @[<hov 2>%a@]@]" 
        term t
  | EUntypeableFix t ->
      fprintf fmt "@[<v 2>A fix need to start with an abstraction which is itself :@ @[<hov 2>%a@]@]" 
        term t
  | EPrjOutOfBound (v, t) ->
      fprintf fmt "@[<v 2>Projection out of bound π %d :@ @[<hov 2>%a@]@]" v term t
  | EPrjLabelNotFound (l, t) ->
      fprintf fmt "@[<v 2>Projection with a label not found@ π %s @[<hov 2>%a@]@]" l term t
  | EPrjNotVrt ts ->
      fprintf fmt "@[<v 2>Projection on something which have not a record type :@ @[<hov 2>%a@]@]" 
        term ts
  | EPrjNotNatOrLbl (t1, t2) ->
      fprintf fmt "@[<v 2>Projection with something with have not a type nat or label 
        and on something which have not a record type@ π @[<hov 2>%a@]@] %a" term t1 term t2
  | ELabelRcdUniq ts ->
      fprintf fmt "@[<v 2>Labels are not unique in this record : :@ @[<hov 2>%a@]@]" term ts
  | ELabelVrtUniq ts ->
      fprintf fmt "@[<v 2>Labels are not unique in this variant : :@ @[<hov 2>%a@]@]" term ts
  | ECaseNotVrt   t ->
      fprintf fmt "@[<v 2>This term have not variant type :@ @[<hov 2>%a@]@]" term t
  | ECaseNotSameVrt (t1, t2) ->
      fprintf fmt "@[<v 2>This two variant have not the same fields:
        t1 =@ @[<hov 2>%a@]@ 
        t2 =@ @[<hov 2>%a@]@] " term t1 term t2

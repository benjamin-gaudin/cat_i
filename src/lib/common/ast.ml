type cond = Ifz | Ifn | If

type uop  = Fix | HD | TL | Abs

type bop  = App | And | Or | Add | Sub | Mul | Con | Prj

type const = Nil | Tru | Fal

type term = Cst of const
          | Lbl of string
          | Nat of int
          | Var of int
          | Rcd of (string * term)list
          | Uop of uop * term
          | Bop of bop * term * term
          | Let of term * term * term
          | Cod of cond * term * term * term


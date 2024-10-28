type cond = Ifz | Ifn | If

type uop  = Fix | HD | TL | Abs

type bop  = App | And | Or | Add | Sub | Mul | Con

type const = Nil | Tru | Fal

type term = Cst of const
          | Nat of int
          | Var of int
          | Uop of uop * term
          | Bop of bop * term * term
          | Let of term * term * term
          | Cod of cond * term * term * term


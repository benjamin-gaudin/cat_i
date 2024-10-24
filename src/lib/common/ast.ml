type cond = Ifz | Ifn

type uop  = Fix | HD | TL | Abs

type bop  = App | Add | Sub | Mul | Con

type term = Nil
          | Nat of int
          | Var of int
          | Uop of uop * term
          | Bop of bop * term * term
          | Let of term * term * term
          | Cod of cond * term * term * term


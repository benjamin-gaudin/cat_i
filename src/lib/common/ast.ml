type cond = Ifz | Ifn

type uop  = Fix | HD | TL | Abs

type bop  = App | Add | Sub | Mul

type term = Nat of int
          | Var of int
          | Lis of tlist
          | Uop of uop * term
          | Bop of bop * term * term
          | Let of term * term * term
          | Cod of cond * term * term * term

and tlist = Nil
          | Con of term * tlist


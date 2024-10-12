{
  open Lexing
  open Parser
  open Error
}

(* let alpha = ['a'-'z'] *)
let num   = ['0'-'9']+
(* let id    = alpha (alpha | '_' | num)* *)
let space = [' ' '\n']+


rule token = parse
  (* Operations ------------------------------------------------------------- *)
  | "λ" | "l" | "L" | "lambda" { LAMBDA                                        }
  (* Special characters ----------------------------------------------------- *)
  | "("      { LPAR                                                            }
  | ")"      { RPAR                                                            }
  | "//"     { comnt lexbuf                                                    }
  | space    { token lexbuf                                                    }
  | ";"      { SEMI                                                            }
  | eof      { EOF                                                             }
  (* Values ----------------------------------------------------------------- *)
  | num as n { INDEX (int_of_string n)                                         }
  (* Unknown ---------------------------------------------------------------- *)
  | _        { raise (E (Lexing_error (lexeme lexbuf)))                        }

and comnt = parse
  | '\n' { token lexbuf }
  | _    { comnt lexbuf }

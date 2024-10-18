{
  open Lexing
  open Parser
  open Error
}

let num   = ['0'-'9']+
let space = [' ' '\n']+


rule token = parse
  (* Operations ------------------------------------------------------------- *)
  | "λ" | "l" | "L" | "lambda" { LAMBDA                                        }
  | "+"                        { PLUS                                          }
  | "*"                        { STAR                                          }
  (* Special characters ----------------------------------------------------- *)
  | "i"        { IDI                                                           }
  | "("        { LPAR                                                          }
  | ")"        { RPAR                                                          }
  | "//"       { comnt lexbuf                                                  }
  | space      { token lexbuf                                                  }
  | ";"        { SEMI                                                          }
  | eof        { EOF                                                           }
  (* Values ----------------------------------------------------------------- *)
  | num as n   { CNAT  (int_of_string n)                                       }
  (* Unknown ---------------------------------------------------------------- *)
  | _          { raise (E (Lexing_error (lexeme lexbuf)))                      }

and comnt = parse
  | '\n' { token lexbuf }
  | _    { comnt lexbuf }

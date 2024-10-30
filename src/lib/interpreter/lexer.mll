{
  open Lexing
  open Parser
  open Common.Error
}

let num   = ['0'-'9']+
let space = [' ' '\n']+


rule token = parse
  (* Operations ------------------------------------------------------------- *)
  | "λ" | "l"  { LAMBDA                                                        }
  | "="        { EQUAL                                                         }
  | "fst"      { FST                                                           }
  | "snd"      { SND                                                           }
  | "&&"       { AND                                                           }
  | "||"       { OR                                                            }
  | "+"        { ADD                                                          }
  | "-"        { SUB                                                           }
  | "*"        { MUL                                                          }
  | "::"       { DCOLON                                                        }
  | "HD"       { HD                                                            }
  | "TL"       { TL                                                            }
  (* Special characters ----------------------------------------------------- *)
  | "i"        { IDI                                                           }
  | "("        { LPAR                                                          }
  | ")"        { RPAR                                                          }
  | "["        { LBRA                                                          }
  | "]"        { RBRA                                                          }
  | "//"       { comnt lexbuf                                                  }
  | space      { token lexbuf                                                  }
  | ";"        { SEMI                                                          }
  | ","        { COMA                                                          }
  | eof        { EOF                                                           }
  (* Keywords --------- ----------------------------------------------------- *)
  | "let"      { LET                                                           }
  | "in"       { IN                                                            }
  | "fix"      { FIX                                                           }
  | "ifz"      { IFZ                                                           }
  | "ifn"      { IFN                                                           }
  | "if"       { IF                                                            }
  | "then"     { THEN                                                          }
  | "else"     { ELSE                                                          }
  (* options ---------------------------------------------------------------- *)
  | "e" | "-e" { OPE                                                           }
  (* Values ----------------------------------------------------------------- *)
  | "true"     { TRUE                                                          }
  | "false"    { FALSE                                                         }
  | num as n   { CNAT (int_of_string n)                                        }
  (* Unknown ---------------------------------------------------------------- *)
  | _          { raise (E (ELexing (lexeme lexbuf)))                      }

and comnt = parse
  | '\n' { token lexbuf }
  | _    { comnt lexbuf }

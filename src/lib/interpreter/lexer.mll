{
  open Lexing
  open Parser
  open Common.Error
}

let chr   = ['a'-'z']
let num   = ['0'-'9']+
let space = [' ' '\n']+
let lbl   = chr (chr | '_' | num)*


rule token = parse
  (* Operations ------------------------------------------------------------- *)
  | "λ" | "l"   { LAMBDA                                                       }
  | "="         { EQUAL                                                        }
  | "<"         { LT                                                           }
  | ">"         { GT                                                           }
  | "prj" | "π" { PRJ                                                          }
  | "&&"        { AND                                                          }
  | "||"        { OR                                                           }
  | "+"         { PLUS                                                         }
  | "-"         { SUB                                                          }
  | "*"         { STAR                                                         }
  | ":"         { COLON                                                        }
  | "::"        { DCOLON                                                       }
  | "HD"        { HD                                                           }
  | "TL"        { TL                                                           }
  (* Special characters ----------------------------------------------------- *)
  | "_"         { IDI                                                          }
  | "->"        { RIGHT                                                        }
  | "|"         { VBAR                                                         }
  | "("         { LPAR                                                         }
  | ")"         { RPAR                                                         }
  | "["         { LSBR                                                         }
  | "]"         { RSBR                                                         }
  | "{"         { LBRA                                                         }
  | "}"         { RBRA                                                         }
  | "//"        { comnt lexbuf                                                 }
  | space       { token lexbuf                                                 }
  | ";"         { SEMI                                                         }
  | ","         { COMA                                                         }
  | "."         { DOT                                                          }
  | eof         { EOF                                                          }
  (* Keywords --------- ----------------------------------------------------- *)
  | "let"       { LET                                                          }
  | "in"        { IN                                                           }
  | "fix"       { FIX                                                          }
  | "ifz"       { IFZ                                                          }
  | "ifn"       { IFN                                                          }
  | "if"        { IF                                                           }
  | "as"        { AS                                                           }
  | "then"      { THEN                                                         }
  | "else"      { ELSE                                                         }
  | "case"      { CASE                                                         }
  | "of"        { OF                                                           }
  (* options ---------------------------------------------------------------- *)
  | "e" | "-e"  { OPE                                                          }
  (* types  ----------------------------------------------------------------- *)
  | "unit"         { UNIT                                                       }
  | "nat"          { NAT                                                       }
  | "bool"         { BOOL                                                      }
  | "∀" | "forall" { FORALL                                                    }
  (* Values ----------------------------------------------------------------- *)
  | "true"      { TRUE                                                         }
  | "false"     { FALSE                                                        }
  | num as n    { CNAT (int_of_string n)                                       }
  | lbl as s    { LBL s                                                        }
  (* Unknown ---------------------------------------------------------------- *)
  | _           { raise (E (ELexing (lexeme lexbuf)))                          }

and comnt = parse
  | '\n' { token lexbuf }
  | _    { comnt lexbuf }

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
  | "prj" | "π" { PRJ                                                          }
  | "&&"        { AND                                                          }
  | "||"        { OR                                                           }
  | "+"         { ADD                                                          }
  | "-"         { SUB                                                          }
  | "*"         { MUL                                                          }
  | "::"        { DCOLON                                                       }
  | "HD"        { HD                                                           }
  | "TL"        { TL                                                           }
  (* Special characters ----------------------------------------------------- *)
  | "_"         { IDI                                                          }
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
  | eof         { EOF                                                          }
  (* Keywords --------- ----------------------------------------------------- *)
  | "let"       { LET                                                          }
  | "in"        { IN                                                           }
  | "fix"       { FIX                                                          }
  | "ifz"       { IFZ                                                          }
  | "ifn"       { IFN                                                          }
  | "if"        { IF                                                           }
  | "then"      { THEN                                                         }
  | "else"      { ELSE                                                         }
  (* options ---------------------------------------------------------------- *)
  | "e" | "-e"  { OPE                                                          }
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

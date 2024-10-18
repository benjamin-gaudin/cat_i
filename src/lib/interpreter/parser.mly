%{
  open Lambda.DeBrujin
%}

(* Special characters *)
%token LPAR RPAR PLUS STAR IDI SEMI EOF

%left PLUS
%left STAR

(* Operations *)
%token LAMBDA

(* Values *)
%token <int> CNAT

%start program
%type <pterm list> program

%%


(* Expressions ---------------------------------------------------------------*)

term:
| ap=appTerm           { ap           }
| LAMBDA t=term        { Abs t        }
| t1=term PLUS t2=term { Add (t1, t2) }
| t1=term STAR t2=term { Mul (t1, t2) }

appTerm:
| t=unitTerm             { t            }
| ap=appTerm ut=unitTerm { App (ap, ut) }

unitTerm:
| LPAR t=term RPAR   { t     }
| IDI i=CNAT         { Var i }
| n=CNAT             { Nat n }

(* Program ------------------------------------------------------------------ *)

program:
| EOF { [] }
| t=term SEMI SEMI  p=program { t::p }

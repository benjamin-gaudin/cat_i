%{
  open Lambda.DeBrujin
%}

(* Special characters *)
%token LPAR RPAR ADD MUL IDI SEMI EOF

%left ADD
%left MUL

(* Operations *)
%token LAMBDA

(* Values *)
%token <int> CNAT

%start program
%type <pterm list> program

%%

(* Expressions ---------------------------------------------------------------*)

term:
| ap=appTerm          { ap           }
| t1=term ADD t2=term { Add (t1, t2) }
| t1=term MUL t2=term { Mul (t1, t2) }
| LAMBDA t=term       { Abs t        }

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

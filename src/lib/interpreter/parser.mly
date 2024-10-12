%{
  open Lambda.DeBrujin
%}

(* Special characters *)
%token LPAR RPAR SEMI EOF

(* Operations *)
%token LAMBDA

(* Values *)
%token <int> INDEX

%start program
%type <pterm list> program

%%

(* Expressions ---------------------------------------------------------------*)

term:
| ap=appTerm    { ap    }
| LAMBDA t=term { Abs t }

appTerm:
| t=unitTerm             { t            }
| ap=appTerm ut=unitTerm { App (ap, ut) }

unitTerm:
| LPAR t=term RPAR  { t     }
| i=INDEX           { Var i }

(* Program ------------------------------------------------------------------ *)

program:
| EOF { [] }
| t=term SEMI SEMI  p=program { t::p }

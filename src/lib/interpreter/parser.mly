%{
  open Lambda.DeBrujin
%}

(* Special characters *)
%token LPAR RPAR LBRA RBRA PLUS STAR IDI DCOLON SEMI EOF

%left PLUS
%left STAR

(* Operations *)
%token LAMBDA HD TL

(* Values *)
%token <int> CNAT

%start program
%type <pterm list> program

%%


(* Expressions ---------------------------------------------------------------*)

term:
| ap=appTerm           { ap           }
| LAMBDA t=term        { Abs t        }


appTerm:
| t=arithTerm            { t            }
| ap=appTerm ut=unitTerm { App (ap, ut) }

arithTerm:
| t=unitTerm                     { t            }
| t1=arithTerm STAR t2=arithTerm { Mul (t1, t2) }
| t1=arithTerm PLUS t2=arithTerm { Add (t1, t2) }

unitTerm:
| LPAR t=term RPAR   { t     }
| IDI i=CNAT         { Var i }
| n=CNAT             { Nat n }
| HD t=unitTerm      { HD t  }
| TL t=unitTerm      { TL t  }
| l=listTerm         { Lis l }

listTerm:
| LBRA RBRA                     {Nil}
| LBRA s=seq RBRA               {plist_of_list s}
| t=unitTerm DCOLON l=listTerm  { Con (t,l) }

seq:
| t=term             { [t]     }
| t=term SEMI ts=seq { t :: ts }

(* Program ------------------------------------------------------------------ *)

program:
| EOF { [] }
| t=term SEMI SEMI  p=program { t::p }

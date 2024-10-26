%{
  open Common.Options
  open Common.Ultils
  open Common.Ast
%}

(* Special characters *)
%token LPAR RPAR LBRA RBRA 
%token DCOLON SEMI
%token IDI EQUAL EOF


(* Operations *)
%token LAMBDA
%token AND OR
%token ADD SUB MUL
%token HD TL

%left ADD SUB
%left MUL
%left AND OR

(* Keywords *)
%token FIX
%token LET IN
%token IFZ IFN IF THEN ELSE

(* Values *)
%token TRUE FALSE
%token <int>    CNAT

(* Options *)
%token OPE

%start program
%type <(term * opt list) list> program

%%

value:
| IDI i=CNAT         { Var i   }
| n=CNAT             { Nat n   }
| TRUE               { Con Tru }
| FALSE              { Con Fal }


bop:
| ADD { Add } | SUB { Sub } | MUL { Mul } | AND { And } | OR  { Or }

condition:
| IF { If } | IFZ { Ifz } | IFN { Ifn }

term:
| ap=appTerm                           { ap                   }
| LAMBDA t=term                        { Uop (Abs, t)         }
| FIX t=term                           { Uop (Fix, t)         }
| LET x=term EQUAL t1=term IN t2=term  { Let (x, t1, t2)      }
| co=condition c=term THEN t1=term ELSE t2=term
  { Cod (co, c, t1, t2) }


appTerm:
| t=bopTerm              { t                  }
| ap=appTerm ut=unitTerm { Bop (App, ap, ut) }

bopTerm:
| t =unitTerm                     { t            }
| t1=bopTerm b=bop  t2=bopTerm { Bop (b, t1, t2) }


unitTerm:
| LPAR t=term RPAR   { t           }
| v=value            { v           }
| HD t=unitTerm      { Uop (HD, t) }
| TL t=unitTerm      { Uop (TL, t) }
| LBRA RBRA          { Con Nil     }
| LBRA s=seq RBRA    { tlist_of_list s }
| t=unitTerm DCOLON ts=unitTerm  { Bop (Con, t,ts)  }

seq:
| t=term             { [t]     }
| t=term SEMI ts=seq { t :: ts }

option:
| OPE { Eq }

options:
| o=option { [o] }
| o=option os=options { o :: os }

(* Program ------------------------------------------------------------------ *)

program:
| EOF { [] }
| t=term SEMI SEMI os=options SEMI p=program { (t, os)::p }
| t=term SEMI SEMI p=program { (t, [])::p }

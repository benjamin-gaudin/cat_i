%{
  open Common.Ast
  open Common.Ultils
%}

(* Special characters *)
%token LPAR RPAR LBRA RBRA IDI DCOLON SEMI EQUAL EOF


(* Operations *)
%token LAMBDA HD TL PLUS SUB STAR

%left PLUS
%left SUB
%left STAR

(* Keywords *)
%token FIX IFZ IFN THEN ELSE LET IN

(* Values *)
%token <int>    CNAT

(* Conditions *)
%token OPE OPF

%start program
%type <(term * opt list) list> program

%%


(* Expressions ---------------------------------------------------------------*)

term:
| ap=appTerm                           { ap                   }
| LAMBDA t=term                        { Uop (Abs, t)         }
| FIX t=term                           { Uop (Fix, t)         }
| LET x=term EQUAL t1=term IN t2=term  { Let (x, t1, t2)      }
| IFZ c=term THEN t1=term ELSE t2=term { Cod (Ifz, c, t1, t2) }
| IFN c=term THEN t1=term ELSE t2=term { Cod (Ifn, c, t1, t2) }


appTerm:
| t=arithTerm            { t                  }
| ap=appTerm ut=unitTerm { Bop (App, ap, ut) }

arithTerm:
| t=unitTerm                     { t            }
| t1=arithTerm STAR t2=arithTerm { Bop (Mul, t1, t2) }
| t1=arithTerm SUB  t2=arithTerm { Bop (Sub, t1, t2) }
| t1=arithTerm PLUS t2=arithTerm { Bop (Add, t1, t2) }

unitTerm:
| LPAR t=term RPAR   { t           }
| IDI i=CNAT         { Var i       }
| n=CNAT             { Nat n       }
| HD t=unitTerm      { Uop (HD, t) }
| TL t=unitTerm      { Uop (TL, t) }
| LBRA RBRA          { Nil             }
| LBRA s=seq RBRA    { tlist_of_list s }
| t=unitTerm DCOLON ts=unitTerm  { Con (t,ts)       }


seq:
| t=term             { [t]     }
| t=term SEMI ts=seq { t :: ts }

(* Program ------------------------------------------------------------------ *)

opt:
| OPF { Fv }
| OPE { Eq }

opts:
| o=opt { [o] }
| o=opt os=opts { o :: os }


program:
| EOF { [] }
| t=term SEMI SEMI os=opts SEMI p=program { (t, os)::p }
| t=term SEMI SEMI p=program { (t, [])::p }

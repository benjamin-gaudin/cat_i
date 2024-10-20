%{
  open Lambda.DeBrujin
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
%type <(pterm * opt list) list> program

%%


(* Expressions ---------------------------------------------------------------*)

term:
| ap=appTerm                           { ap              }
| LAMBDA t=term                        { Abs t           }
| FIX t=term                           { Fix t           }
| LET x=term EQUAL t1=term IN t2=term  { Let (x, t1, t2) }
| IFZ c=term THEN t1=term ELSE t2=term { Ifz (c, t1, t2) }
| IFN c=term THEN t1=term ELSE t2=term { Ifn (c, t1, t2) }


appTerm:
| t=arithTerm            { t            }
| ap=appTerm ut=unitTerm { App (ap, ut) }

arithTerm:
| t=unitTerm                     { t            }
| t1=arithTerm STAR t2=arithTerm { Mul (t1, t2) }
| t1=arithTerm SUB  t2=arithTerm { Sub (t1, t2) }
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

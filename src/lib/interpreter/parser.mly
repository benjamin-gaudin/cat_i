%{
  open Common.Options
  open Common.Utils
  open Common.Ast
%}

(* Special characters *)
%token LPAR RPAR LSBR RSBR LBRA RBRA
%token COMA SEMI
%token IDI EQUAL EOF


(* Operations *)
%token PRJ
%token DCOLON
%token LAMBDA
%token AND OR
%token ADD SUB MUL
%token HD TL

%left ADD SUB
%left MUL
%left AND OR
%left DCOLON
%right PRJ

(* Keywords *)
%token FIX
%token LET IN
%token IFZ IFN IF THEN ELSE

(* Values *)
%token TRUE FALSE
%token <int>    CNAT
%token <string> LBL

(* Options *)
%token OPE

%start program
%type <(term * opt list) list> program

%%

value:
| IDI i=CNAT { Var i   }
| n=CNAT     { Nat n   }
| TRUE       { Cst Tru }
| FALSE      { Cst Fal }

%inline uop:
LAMBDA { Abs } | FIX { Fix } | HD { HD } | TL { TL }

%inline bop:
| ADD { Add } | SUB { Sub } | MUL { Mul } | AND { And } | OR { Or } | 
  PRJ { Prj }

%inline cond:
| IF { If } | IFZ { Ifz } | IFN { Ifn }

term:
| ap=appTerm                               { ap                       }
| u=uop t=term                             { Uop (u, t)               }
| PRJ i=LBL rs=unitTerm            { Bop (Prj, Lbl i, rs) }
| PRJ t=unitTerm  rs=unitTerm       { Bop (Prj, t,  rs)     }
// | PRJ t=unitTerm LPAR rs=unitTerm RPAR        { Bop (Prj, t,  rs)     }
| LET x=term EQUAL t1=term IN t2=term      { Let (x, t1, t2)          }
| co=cond c=term THEN t1=term ELSE t2=term { Cod (co, c, t1, t2)      }

appTerm:
| t=bopTerm              { t                  }
| ap=appTerm ut=unitTerm { Bop (App, ap, ut) }

bopTerm:
| t =unitTerm                  { t               }
| t1=bopTerm b=bop  t2=bopTerm { Bop (b, t1, t2) }

unitTerm:
| LPAR t=term RPAR              { t               }
| LPAR ts=tuple RPAR            { Rcd ts          }
| LBRA ts=record RBRA           { Rcd ts          }
| v=value                       { v               }
| LSBR RSBR                     { Cst Nil         }
| LSBR s=seq RSBR               { tlist_of_list s }
| t=unitTerm DCOLON ts=unitTerm { Bop (Con, t,ts) }

recordField:
| l=LBL EQUAL t=term { (l,t)  }
| t=term             { ("",t) }

record:
| r=recordField                { [r]     }
| r=recordField COMA rs=record { r :: rs }

tuple:
| t=term COMA          { [("", t)]           }
| t=term COMA t2=term  { [("", t); ("", t2)] }
| t=term COMA ts=tuple { ("", t) :: ts       }

seq:
| t=term             { [t]     }
| t=term SEMI ts=seq { t :: ts }

opt:
| OPE { Eq }

opts:
| o=opt { [o] }
| o=opt os=opts { o :: os }

program:
| EOF { [] }
| t=term SEMI SEMI os=opts SEMI p=program { (t, os)::p }
| t=term SEMI SEMI p=program { (t, [])::p }

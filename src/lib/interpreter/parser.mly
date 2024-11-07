%{
  open Common.Options
  open Common.Utils
  open Common.Ast
%}

(* Special characters *)
%token LPAR RPAR LSBR RSBR LBRA RBRA
%token COMA SEMI DOT COLON
%token IDI EOF
%token RIGHT
%token VBAR


(* Operations *)
%token PRJ
%token DCOLON
%token LAMBDA
%token AND OR
%token EQUAL GT LT
%token PLUS SUB STAR
%token HD TL

%left PLUS SUB
%left STAR
%left AND OR
%left DCOLON
%right PRJ
%right RIGHT

(* Keywords *)
%token FIX
%token LET IN
%token IFZ IFN IF THEN ELSE 
%token CASE OF
%token AS
%token FORALL

%left AS

(* types *)
%token NAT BOOL UNIT

(* Values *)
%token TRUE FALSE
%token <int>    CNAT
%token <string> LBL

(* Options *)
%token OPE

%start program
%type <(term * opt list) list> program

%%

lbl_typ_list :
| l=LBL COLON ty=ptype                      { [(l, ty)]     }
| l=LBL COLON ty=ptype COMA ls=lbl_typ_list { (l, ty) :: ls }

ptype:
| UNIT                                { Cst Uni            }
| NAT                                 { Cst Nat            }
| BOOL                                { Cst Bol            }
| l=LBL                               { Gen (Var l ,Var l) }
| LSBR ty=ptype RSBR                  { Lis ty             }
| LBRA tys=lbl_typ_list RBRA          { Rcd tys            }
| LT tys=lbl_typ_list GT              { Vrt tys            }
| FORALL ty1=ptype DOT ty2=ptype      { Arr (ty1, ty2)     }
| LPAR ty1=ptype RIGHT ty2=ptype RPAR { Arr (ty1, ty2)     }

value:
| LPAR RPAR  { Cst Uni }
| IDI i=CNAT { Var i   }
| n=CNAT     { Nat n   }
| TRUE       { Cst Tru }
| FALSE      { Cst Fal }

%inline uop:
LAMBDA { Abs } | FIX { Fix } | HD { HD } | TL { TL }

%inline bop:
| PLUS { Add } | SUB { Sub } | STAR { Mul } | AND { And } | OR { Or } | 
  PRJ { Prj }

%inline cond:
| IF { If } | IFZ { Ifz } | IFN { Ifn }

term:
| ap=app_term                              { ap                   }
| u=uop t=term                             { Uop (u, t)           }
| LPAR t=term AS ty=ptype RPAR             { As  (t, ty)          }
| PRJ i=LBL rs=unit_term                   { Bop (Prj, Lbl i, rs) }
| PRJ t=unit_term  rs=unit_term            { Bop (Prj, t,  rs)    }
| LET x=term EQUAL t1=term IN t2=term      { Let (x, t1, t2)      }
| co=cond c=term THEN t1=term ELSE t2=term { Cod (co, c, t1, t2)  }
| CASE t=term OF ts=case_variant           { Cas (t, ts)          }

app_term:
| t= bop_term              { t                  }
| ap=app_term ut=unit_term { Bop (App, ap, ut)  }

bop_term:
| t= unit_term                   { t               }
| t1=bop_term b=bop  t2=bop_term { Bop (b, t1, t2) }

unit_term:
| LPAR t=term RPAR                { t               }
| LPAR ts=tuple RPAR              { Rcd ts          }
| LBRA ts=record RBRA             { Rcd ts          }
| LT ts=seq_lbl_eq_term GT        { Vrt ts          }
| v=value                         { v               }
| LSBR RSBR                       { Cst Nil         }
| LSBR s=seq RSBR                 { tlist_of_list s }
| t=unit_term DCOLON ts=unit_term { Bop (Con, t,ts) }

record_field:
| l=LBL EQUAL t=term { (l,t)  }
| t=term             { ("",t) }

record:
| r=record_field                { [r]     }
| r=record_field COMA rs=record { r :: rs }

tuple:
| t=term COMA          { [("", t)]           }
| t=term COMA t2=term  { [("", t); ("", t2)] }
| t=term COMA ts=tuple { ("", t) :: ts       }


seq_lbl_eq_term:
| l=LBL EQUAL t=term                         { [(l,t)]     }
| l=LBL EQUAL t=term COMA ts=seq_lbl_eq_term { (l,t) :: ts }

case_variant:
| LT l=LBL GT RIGHT t=term                           { [(l, t)]     }
// | VBAR LT l=LBL GT RIGHT t=term                      { [(l, t)]     }
| VBAR LT l=LBL GT RIGHT t=term VBAR vs=case_variant { (l, t) :: vs }

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

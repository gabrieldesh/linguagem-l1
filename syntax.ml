type variable = string

(* Outros operadores binário e unários podem ser adicionados a linguagem *) 


type operator = Sum | Diff | Mult | Div | Eq | And | Or | NotEqual | Less 
              | Greater | LessOrEqual | GreaterOrEqual

type tipo  = TyVar of variable | TyInt | TyBool | TyFn of tipo * tipo 
           | TyList of tipo


type expr = Num of int 
          | Bool of bool 
          | Bop of operator * expr * expr
          | Not of expr
          | If of expr * expr * expr 
          | Var of variable 
          | App of expr * expr 
          | Lam of variable * tipo * expr
          | LamImpl of variable * expr
          | Let of variable * tipo * expr * expr
          | LetImpl of variable * expr * expr
          | Lrec of variable * tipo * tipo * variable * tipo * expr * expr
          | LrecImpl of variable * variable * expr * expr
          | Nil
          | Cons of expr * expr
          | IsEmpty of expr
          | Hd of expr
          | Tl of expr
          | Raise
          | Try of expr * expr

type result = Vnum of int 
           | Vbool of bool 
           | Vnil
           | Vcons of result * result 
           | Vclos of variable * expr * env
           | Vrclos of variable * variable * expr * env
           | RRaise
and  
     env = (variable * result) list
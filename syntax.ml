type variable = string

(* Outros operadores binário e unários podem ser adicionados a linguagem *) 


type operator = Sum | Diff | Mult | Div | Eq | And | Or 

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

type value = Vnum of int 
           | Vbool of bool 
           | Vnil
           | Vcons of value * value 
           | Vclos of variable * expr * env
           | Vrclos of variable * variable * expr * env
and  
     env = (variable * value) list 




(* Segue um exemplo de como o programa L1 abaixo pode ser representado internamente *)

(* let rec fat: int -> int = (fn x: int => if (x == 0) then 1 else x * (fat (x - 1)))
   in fat (5)
   end
*)

let example = Lrec("fat", TyInt, TyInt, "x", TyInt,
              If(Bop(Eq, Var("x"), Num(0)),
                 Num(1),
                 Bop(Mult, Var("x"), App(Var("fat"), Bop(Diff, Var("x"), Num(1))))),
              App(Var("fat"), Num(5)))

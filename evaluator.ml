type variable = string

(* Outros operadores binário e unários podem ser adicionados a linguagem *) 


type operator = Sum | Diff | Mult | Div | Eq | And | Or | NotEqual | Less | Greater | LessOrEqual | GreaterOrEqual

type tipo  = TyVar of variable | TyInt | TyBool | TyFn of tipo * tipo 
           | TyList of tipo
		   
		   
		
(* Excecao a ser ativada quando termo for uma FORMA NORMAL *)
exception NoRuleApplies

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
	 
	 

let rec isValue exp = 
match exp with
	  Vnum _ -> true
	| Vbool _ -> true
	| Vnil _ -> true
	| Vcons(e1,e2) -> isValue(e1) && isValue(e2)
	| Vclos(_) -> true
	| exp -> false
	
	
let rec eval exp =
	match exp with
	Num n -> n
	| Bool b -> b
	| Bop(_,Raise,_) -> Raise
	| Bop(op,v,Raise) when isValue(v) -> Raise
	| Bop(op,e1,e2) when isValue(eval(e1)) && isValue(eval(e2)) ->
		(match op with
			  Sum -> eval(e1) + eval(e2)
			| Diff -> eval(e1) - eval(e2)
			| Mult -> eval(e1) * eval(e2)
			| Div ->(match eval(e2) with
					0 -> Raise
					| _ -> eval(e1) / eval(e2)
					)
			| Eq -> eval(e1) == eval(e2)
			| And -> eval(e1) && eval(e2)
			| Or -> eval(e1) || eval(e2)
			| NotEqual -> eval(e1) != eval(e2)
			| Less -> eval(e1) < eval(e2)
			| Greater -> eval(e1) > eval(e2)
			| LessOrEqual -> eval(e1) <= eval(e2)
			| GreaterOrEqual -> eval(e1) >= eval(e2)
		)
	(*Não tenho certeza se precisa dessas:  *)
	| Bop(op,e1,e2) when eval(e1) == Raise -> Raise
	| Bop(op,e1,e2) when eval(e2) and isValue(eval(e1)) == Raise -> Raise
	(*Fim da incerteza hehe*)
	
	| Not(e1) ->(match eval(e1) with
				 Raise -> Raise
				| _ -> !eval(e1)
				)
				
	| If(e1,e2,e3) when eval(e1) == Raise -> Raise
	| If(e1,e2,e3) when eval(e1) == true ->(match eval(e2) with
											Raise -> Raise
											| _ -> eval(e2)
											)
	| If(e1,e2,e3) when eval(e1) == false ->(match eval(e3) with
											 Raise -> Raise
											 | _ -> eval(e3)
											 )
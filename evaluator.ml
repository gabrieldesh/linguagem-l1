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
	 
	 
	 
let empty_env : env = []


let remove_tuple var list =
  List.filter (fun (k,_) -> k <> var) list 
  
  
let update_env var v1 env : env = match env with
	[] -> [(var,v1)]
	| hd::tl ->
		if (List.exists (fun (k,_) -> k = var) env)
			then List.append (remove_tuple var env) [(var,v1)]
			else List.append env [(var,v1)]

		
let rec lookup_environment var env : value = match env with
	| [] -> raise Not_found
	| (k,v)::tl ->
		if (k = var)
			then v
			else lookup_environment var tl
				
				
let rec isValue exp = 
match exp with
	  Vnum _ -> true
	| Vbool _ -> true
	| Vnil _ -> true
	| Vcons(e1,e2) -> isValue(e1) && isValue(e2)
	| Vclos(_) -> true
	| exp -> false
	
	
let rec eval (env:env) (exp : expr) : value =	match exp with
	(* Valores *)
	Num(n) -> VNum(n)
	| Bool(b) -> VBool(b)

	(* Operações *)
	
	(* Operações binárias *)
	(* O primeiro operando avalia para Raise *)
	| Bop(op,e1,e2) when eval(env e1) == Raise -> Raise
	(* O segundo operando avalia para Raise *)	
	| Bop(op,e1,e2) when isValue(eval(env e1)) && eval(env e2) == Raise -> Raise
	(* Nenhum dos operandos avalia para raise *)
	| Bop(op,e1,e2) ->
		let n1 = eval env e1 in
		let n2 = eval env e2 in
		(match op with
			  Sum -> n1 + n2
			| Diff -> n1 - n2
			| Mult -> n1 * n2
			| Div ->(match n2 with
					0 -> Raise
					| _ -> n1 / n2
					)
			| Eq -> n1 == n2
			| And -> n1 && n2
			| Or -> n1 || n2
			| NotEqual -> n1 != n2
			| Less -> n1 < n2
			| Greater -> n1 > n2
			| LessOrEqual -> n1 <= n2
			| GreaterOrEqual -> n1 >= n2
		)
	(* Not *)
	| Not(e1) ->(match eval(env e1) with
				 Raise -> Raise
				| _ -> !eval(e1)
				)
				
				
	(* If *)
	| If(e1,e2,e3) when eval(env e1) == Raise -> Raise
	| If(e1,e2,e3) when eval(env e1) == true ->(match eval(env e2) with
											Raise -> Raise
											| _ -> eval(env e2)
											)
	| If(e1,e2,e3) when eval(env e1) == false ->(match eval(env e3) with
											 Raise -> Raise
											 | _ -> eval(env e3)
											 )
											 
	(* Variável *)
	| Var(variable) -> lookup_environment variable environment
	
	(* Aplicação *)
	| App(e1,e2) when eval(env e1) == Raise -> Raise
	| App(e1,e2) when isValue(eval(env e1)) && (eval(env e2) == Raise) -> Raise
	| App(e1,e2) -> 
		let v1 = eval env e1 in
		let v2 = eval env e2 in
		(match v1,v2 with
			Vclos(var,e,env), v -> 
				if(eval(update_env var v env) e == Raise) 
					then Raise 
					else eval(update_env var v env) e
				
		|	Vrclos(f,x,e,enf), v -> 
				if(eval(update_env f (Vrclos(f,x,e,env)) (update_env x v env)) e == Raise)
					then Raise
					else eval(update_env f (Vrclos(f,x,e,env)) (update_env x v env)) e
		)
	
	(* Função - Lam *)
	
	(* Let *)
	
	(* LRec *)
	
	(* Nil *)
	(* Cons *)
	(* IsEmpty *)
	(* Hd *)
	(* Tl *)
	(* Try *)
	(* Raise - talvez lançar alguma exceção efetiva, algo do tipo
	| Raise -> raise Exception
	*)
	(*
		  | Nil
          | Cons of expr * expr
          | IsEmpty of expr
          | Hd of expr
          | Tl of expr
          | Raise
          | Try of expr * expr
	*)
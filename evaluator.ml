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

type result = Vnum of int
           | Vbool of bool
           | Vnil
           | Vcons of result * result
           | Vclos of variable * expr * env
           | Vrclos of variable * variable * expr * env
           | RRaise
and
     env = (variable * result) list


let remove_tuple var list =
  List.filter (fun (k,_) -> k <> var) list


let update_env var v1 env : env = match env with
	[] -> [(var,v1)]
	| hd::tl ->
		if (List.exists (fun (k,_) -> k = var) env)
			then List.append (remove_tuple var env) [(var,v1)]
			else List.append env [(var,v1)]


let lookup_environment = List.assoc


let rec eval (env:env) (exp : expr) : result =	match exp with
	(* Valores *)
	Num(n) -> Vnum(n)
	| Bool(b) -> Vbool(b)

	(* Operações *)

	(* Operações binárias *)
	| Bop(op,e1,e2) ->
      let n1 = eval env e1 in
    	(* O primeiro operando avalia para Raise *)
      if n1 == RRaise then RRaise else
      let n2 = eval env e2 in
	    (* O segundo operando avalia para Raise *)
      if n2 == RRaise then RRaise else
    	(* Nenhum dos operandos avalia para raise *)

		(match op,n1,n2 with
			  Sum,Vnum(n1),Vnum(n2) -> Vnum(n1 + n2)
			| Diff,Vnum(n1),Vnum(n2) -> Vnum(n1 - n2)
			| Mult,Vnum(n1),Vnum(n2) -> Vnum(n1 * n2)
			| Div,Vnum(n1),Vnum(n2) ->(match n2 with
					0 -> RRaise
					| _ -> Vnum(n1 / n2)
					)
			| Eq,Vnum(n1),Vnum(n2) -> Vbool(n1 == n2)
			| And,Vbool(n1),Vbool(n2) -> Vbool(n1 && n2)
			| Or,Vbool(n1),Vbool(n2) -> Vbool(n1 || n2)
			| NotEqual,Vnum(n1),Vnum(n2) -> Vbool(n1 != n2)
			| Less,Vnum(n1),Vnum(n2) -> Vbool(n1 < n2)
			| Greater,Vnum(n1),Vnum(n2) -> Vbool(n1 > n2)
			| LessOrEqual,Vnum(n1),Vnum(n2) -> Vbool(n1 <= n2)
			| GreaterOrEqual,Vnum(n1),Vnum(n2) -> Vbool(n1 >= n2)
		)
	(* Not *)
	| Not(e1) ->
		let v1 = eval env e1 in
		if v1 == RRaise then RRaise
			else if v1 == Vbool(true) then Vbool(false)
				else Vbool(true)


	(* If *)
	| If(e1,e2,e3) when eval env e1 == RRaise -> RRaise
	| If(e1,e2,e3) when eval env e1 == Vbool(true) ->
		let v2 = eval env e2 in
		(match v2 with
			RRaise -> RRaise
			| _ -> v2
		)
	| If(e1,e2,e3) when eval env e1 == Vbool(false) ->
		let v3 = eval env e3 in
		(match v3 with
			RRaise -> RRaise
			| _ -> v3
		)

	(* Variável *)
	| Var(variable) -> lookup_environment variable env


	(* Aplicação *)
	| App(e1,e2) ->
		let v1 = eval env e1 in
		if v1 == RRaise then RRaise else
		let v2 = eval env e2 in
		if v2 == RRaise then RRaise else

		(match v1,v2 with
			Vclos(var,e,env), v ->
				let n = eval (update_env var v env) e in
				if(n == RRaise)
					then RRaise
					else n

			| Vrclos(f,x,e,env), v ->
				let n_rec = eval (update_env f (Vrclos(f,x,e,env)) (update_env x v env)) e in
				if(n_rec == RRaise)
					then RRaise
					else n_rec
		)


	(* Função - Lam // Lam Implícito *)
	| LamImpl(variable,exp) -> Vclos(variable,exp,env)
	| Lam(variable,tipo,exp) -> Vclos(variable,exp,env)

	(* Let // Let Implícito *)
	| Let(var,tipo,e1,e2) ->
	let v1 = eval env e1 in
	if v1 == RRaise then RRaise else
		eval (update_env var v1 env) e2

	| LetImpl(var,e1,e2) ->
	let v1 = eval env e1 in
	if v1 == RRaise then RRaise else
		eval (update_env var v1 env) e2

	(* LRec *)
(*| Lrec of variable * tipo * tipo * variable * tipo * expr * expr
**| LrecImpl of variable * variable * expr * expr
*)
	| Lrec(varF,t1,t2,varIN,tIN,e1,e2) ->
		let v1 = eval env e1 in
		if v1 == RRaise then RRaise else
			(match v1 with
				Vclos(varIN,e,env') -> eval (update_env varF (Vrclos(varF,varIN,e,env)) env') e2
			)
	| LrecImpl(varF,varIN,e1,e2) ->
		let v1 = eval env e1 in
		if v1 == RRaise then RRaise else
			(match v1 with
				Vclos(varIN,e,env') -> eval (update_env varF (Vrclos(varF,varIN,e,env)) env') e2
			)

	(* Nil *)
    | Nil -> Vnil

    (* Cons *)
    | Cons(elemento, lista) when eval env elemento == RRaise -> RRaise
    | Cons(elemento, lista) when eval env lista == RRaise -> RRaise
    | Cons(elemento, lista) -> Vcons(eval env elemento, eval env lista)

    (* IsEmpty *)
    | IsEmpty(e1) when eval env e1 == RRaise -> RRaise
    | IsEmpty(e1) ->
            if eval env e1 == Vnil
            then Vbool(true)
            else Vbool(false)

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

open Syntax

(* Excecao a ser ativada quando nenhuma regra se aplica. *)
exception NoRuleApplies

exception UndefinedIdentifier of variable

let remove_tuple var list =
  List.filter (fun (k,_) -> k <> var) list


let update_env var v1 env : env = match env with
	[] -> [(var,v1)]
	| hd::tl ->
		if (List.exists (fun (k,_) -> k = var) env)
			then List.append (remove_tuple var env) [(var,v1)]
			else List.append env [(var,v1)]


let lookup_environment = List.assoc

let empty_env : env = []

let rec eval (env:env) (exp : expr) : result =	match exp with
	(* Valores *)
	Num(n) -> Vnum(n)
	| Bool(b) -> Vbool(b)

	(* Operações *)

	(* Operações binárias *)
	| Bop(op,e1,e2) ->
      let n1 = eval env e1 in
    	(* O primeiro operando avalia para Raise *)
      if n1 = RRaise then RRaise else
      let n2 = eval env e2 in
	    (* O segundo operando avalia para Raise *)
      if n2 = RRaise then RRaise else
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
			| _,_,_ -> raise NoRuleApplies
		)
	(* Not *)
	| Not(e1) ->
		let v1 = eval env e1 in
		if v1 = RRaise then RRaise
			else if v1 = Vbool(true) then Vbool(false)
				else Vbool(true)


	(* If *)
	| If(e1,e2,e3) ->
		let b = eval env e1 in
		if b = RRaise then RRaise else
			if b = Vbool(true) then
				let v1 = eval env e2 in
				if v1 = RRaise then RRaise else v1
			else
				let v2 = eval env e3 in
				if v2 = RRaise then RRaise else v2

	(* Variável *)
	| Var(variable) ->
      (try lookup_environment variable env
       with Not_found -> raise (UndefinedIdentifier variable))


	(* Aplicação *)
	| App(e1,e2) ->
		let v1 = eval env e1 in
		if v1 = RRaise then RRaise else
		let v2 = eval env e2 in
		if v2 = RRaise then RRaise else

		(match v1,v2 with
			Vclos(var,e,env), v ->
				let n = eval (update_env var v env) e in
				if(n = RRaise)
					then RRaise
					else n

			| Vrclos(f,x,e,env), v ->
				let n_rec = eval (update_env f (Vrclos(f,x,e,env)) (update_env x v env)) e in
				if(n_rec = RRaise)
					then RRaise
					else n_rec
			| _ -> raise NoRuleApplies
		)


	(* Função - Lam // Lam Implícito *)
	| LamImpl(variable,exp) -> Vclos(variable,exp,env)
	| Lam(variable,tipo,exp) -> Vclos(variable,exp,env)

	(* Let // Let Implícito *)
	| Let(var,tipo,e1,e2) ->
	let v1 = eval env e1 in
	if v1 = RRaise then RRaise else
		eval (update_env var v1 env) e2

	| LetImpl(var,e1,e2) ->
	let v1 = eval env e1 in
	if v1 = RRaise then RRaise else
		eval (update_env var v1 env) e2

	(* LRec *)
	| Lrec(varF,t1,t2,varX,tX,e1,e2) ->
		let v = eval (update_env varF (Vrclos (varF, varX, e1, env)) env) e2 in
		if v = RRaise then RRaise else v

	| LrecImpl(varF,varX,e1,e2) ->
		let v = eval (update_env varF (Vrclos (varF, varX, e1, env)) env) e2 in
		if v = RRaise then RRaise else v

	(* Nil *)
    | Nil -> Vnil

    (* Cons *)
	| Cons(elemento,lista) ->
		let n = eval env elemento in
		if n = RRaise then RRaise
		else
			let n_lista = eval env lista in
			if n_lista = RRaise then RRaise
			else Vcons(n,n_lista)

    (* IsEmpty *)
	| IsEmpty(e1) ->
		let n = eval env e1 in
		if n = RRaise then RRaise
		else if n = Vnil then Vbool(true) else Vbool(false)

	(* Hd *)
	| Hd(l) ->
		let v = eval env l in
		if v = RRaise || v = Vnil then RRaise else
			(match v with Vcons(e1, e2) -> e1
						  | _ -> raise NoRuleApplies
			)

	(* Tl *)
	| Tl(l) ->
	let v = eval env l in
	if v = RRaise || v = Vnil then RRaise else
			(match v with Vcons(e1, e2) -> e2
						  | _ -> raise NoRuleApplies
			)

	(* Try *)
	| Try(e1,e2) ->
	let v1 = eval env e1 in
	if v1 = RRaise then
		let v2 = eval env e2 in
		if v2 = RRaise then RRaise
		else v2
	else v1

	(* Raise *)
    | Raise -> RRaise

	let environment = empty_env;;

	(* Test *)
	let numAccept = Vnum(7);;
	let boolAccept = Vbool(true);;

	(* testando update_env *)
	let env = update_env "numAccept" numAccept environment;;
	lookup_environment "numAccept" env;;

	(* operadores *)
	let sumAccept = Bop(Sum,Num(1),Num(1))
	let sumRaise = Bop(Sum,Raise,Num(2))

	let divAccept = Bop(Div,Num(4),Num(2))
	let divRaise = Bop(Div,Num(2),Raise)
	let divRaise0 = Bop(Div,Num(3),Num(0))
	(* casos como Bop(Sum,Num(1),Bool(true)) serão impedidos pelo avaliador de tipos *)


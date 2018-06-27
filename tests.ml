(* Modulo de testes *)
#mod_use "syntax.ml";;
#mod_use "typeConstraints.ml";;
#mod_use "collectTyEqs.ml";;
#mod_use "examples.ml";;
#mod_use "substituition.ml";;
#mod_use "unify.ml";;
#mod_use "typeInfer.ml";;
#mod_use "evaluator.ml";;

open Syntax;;
open TypeConstraints;;
open CollectTyEqs;;
open Examples;;
open Substituition;;
open Unify;;
open TypeInfer;;
open Evaluator;;

	let environment = empty_env;;
	
	(* Testes - cada um corresponde à uma regra da semântica BIG-STEP *)
	(* BS-NUM *)
	let numAccept = Num(7);;
	let evalNumAccept = eval environment numAccept;;
	
	(* BS-BOOL *)
	let boolAccept = Bool(true);;
	let evalBoolAccept = eval environment boolAccept;;
	
	(* testando update_env *)
	let env = update_env "numAccept" (Vnum(7)) environment;;
	lookup_environment "numAccept" env;;
	
	(* OPERADORES *)
  
	(* BS-OP+ *)
	let sumAccept = Bop(Sum,Num(1),Num(1));;
	let evalSumAccept = eval environment sumAccept;;
		
	(* BS-OP+RS1 *)
	let sumRaise1 = Bop(Sum,Raise,Num(3));;
	let evalSumRaise1 = eval environment sumRaise1;;
	
	(* BS-OP+RS2 *)
	let sumRaise2 = Bop(Sum,Num(2),Raise);;
	let evalSumRaise2 = eval environment sumRaise2;;
	
	(* BS-OPDIV *)
	let divAccept = Bop(Div,Num(4),Num(2));;
	let evalDivAccept = eval environment divAccept;;
  
	(* BS-OPDIVRS1 *)
	let divRaise1 = Bop(Div,Raise,Num(3));;
	let evalDivRaise1 = eval environment divRaise1;;
  
	(* BS-OPDIVRS2 *)
	let divRaise2 = Bop(Div,Num(2),Raise);;
	let evalDivRaise2 = eval environment divRaise2;;
  
	(* BS-OPDIVZERO *)
	let divRaise0 = Bop(Div,Num(3),Num(0));;
	let evalDivRaise0 = eval environment divRaise0;;
	
	(* BS-OP==TR *)
	let eqAccept = Bop(Eq,Num(2),Num(2));;
	let evalEqAccept = eval environment eqAccept;;
  
	(* BS-OP==FLS *)
	let eqReject = Bop(Eq,Num(2),Num(3));;
	let evalEqReject = eval environment eqReject;;
  
	(* BS-OP==RS1 *)
	let eqRaise1 = Bop(Eq,Raise,Num(2));;
	let evalEqRaise1 = eval environment eqRaise1;;
  
	(* BS-OP==RS2 *)
	let eqRaise2 = Bop(Eq,Num(2),Raise);;
	let evalEqRaise2 = eval environment eqRaise2;;
	
	(* BS-OPANDTR *)
	let andTrue = Bop(And,Bool(true),Bool(true));;
	let evalAndTrue = eval environment andTrue;;
  
	(* BS-OPANDFLS *)
	let andFalse = Bop(And,Bool(true),Bool(false));;
	let evalAndFalse = eval environment andFalse;;
  
	(* BS-OPANDRS1 *)
	let andRaise1 = Bop(And,Raise,Bool(true));;
	let evalAndRaise1 = eval environment andRaise1;;
  
	(* BS-OPANDRS2 *)
	let andRaise2 = Bop(And,Bool(true),Raise);;
	let evalAndRaise2 = eval environment andRaise2;;
	
	(* BS-OPNOTTR *)
	let notTrue = Not(Bool(true));;
	let evalNotTrue = eval environment notTrue;;
  
	(* BS-OPNOTFLS *)
	let notFalse = Not(Bool(false));;
	let evalNotFalse = eval environment notFalse;;
  
	(* BS-OPNOTRS *)
	let notRaise = Not(Raise);;
	let evalNotRaise = eval environment notRaise;;
	
	
	(* BS-IFTR *)
	let ifTrue = If(Bop(Eq,Num(1),Num(1)),Bool(true),Bool(false));;
	let evalIfTrue = eval environment ifTrue;;
  
	(* BS-IFFLS *)
	let ifFalse = If(Bop(Eq,Num(1),Num(2)),Bool(true),Bool(false));;
	let evalIfFalse = eval environment ifFalse;;
  
	(* BS-IFRS1 *)
	let ifRaise1 = If(Raise,Bool(true),Bool(false));;
	let evalIfRaise1 = eval environment ifRaise1;;
  
	(* BS-IFRS2 *)
	let ifRaise2 = If(Bop(Eq,Num(1),Num(1)),Raise,Bool(false));;
	let evalIfRaise2 = eval environment ifRaise2;;
  
	(* BS-IFRS3 *)
	let ifRaise3 = If(Bop(Eq,Num(1),Num(2)),Bool(true),Raise);;
	let evalIfRaise3 = eval environment ifRaise3;;
	
	(* BS-FN *)
	let funcAccept = Lam("x", TyInt ,Bop(Sum,Var "x",Num(1)));;
	let evalFuncAccept = eval environment funcAccept;;
	
	(* BS-LET x = x + 1*)
	let letAccept = Let("x", TyInt ,Num(1), Bop(Sum,Var("x"),Num(1)));;
	let evalLetAccept = eval environment letAccept;;
  
	(* BS-LETRS1 *)
	let letRaise1 = Let("x", TyInt ,Raise, Bop(Sum,Var("x"),Num(1)));;
	let evalLetRaise1 = eval environment letRaise1;;
  
	(* BS-LETRS2 *)
	let letRaise2 = Let("x", TyInt ,Num(1), Raise);;
	let evalLetRaise2 = eval environment letRaise2;;
	
	(* BS-LETREC *)
	(* LRec será testado no examples *)
	(* BS-LETRECRS - exemplo fat do examples.ml *)
	let lrecRaise = Lrec ("fat", TyInt, TyInt, "n", TyInt, If (Bop (Eq, Var "n", Num 0),
            Num 1,
            Bop (Mult, Var "n", App (Var "fat", Bop (Diff, Var "n", Num 1)))), Raise);;
	let evalLrecRaise = eval environment lrecRaise;;
	
	
	(* BS-APP x = x + 1 <---- x = 4 RESULTADO = X = 4 + 1*)
	let appAccept = App(Lam("x", TyInt ,Bop(Sum,Var "x",Num(1))),Num(4));;
	let evalAppAccept = eval environment appAccept;;
  
	(* BS-APPRS1 *)
	let appRaise1 = App(Raise,Num(4));;
	let evalAppRaise1 = eval environment appRaise1;;
  
	(* BS-APPRS2 *)
	let appRaise2 = App(Lam("x", TyInt ,Bop(Sum,Var "x",Num(1))),Raise);;
	let evalAppRaise2 = eval environment appRaise2;;
  
	(* BS-APPRS3 *)
	(* let appRaise3 = *)

	let empty_env : env = [];;
	(* Nome da regra - Teste *)
	(* Teste Nil *)
	(* BS-NIL *)
	let exp = Nil;;
	eval empty_env exp;;

	(* Testes listas *)
	(* BS-CONS *)
	let exp = Cons(Num(1), Nil);;
	eval empty_env exp;;

	(* BS-CONSRS1 *)
	let exp = Cons(Raise, Nil);;
	eval empty_env exp;;

	(* BS-CONSRS2 *)
	let exp = Cons(Num(1), Raise);;
	eval empty_env exp;;

	(* BS-ISEMPTYNIL *)
	let exp = IsEmpty(Nil);;
	eval empty_env exp;;

	(* BS-ISEMPTYCONS *)
	let exp = IsEmpty(Cons(Num(1), Nil));;
	eval empty_env exp;;

	(* BS-ISEMPTYRS *)
	let exp = IsEmpty(Raise);;
	eval empty_env exp;;

	(* BS-HDNIL *)
	let exp = Hd(Nil);;
	eval empty_env exp;;

	(* BS-HDCONS *)
	let exp = Hd(Cons(Num(1), Nil));;
	eval empty_env exp;;

	(* BS-HDRS *)
	let exp = Hd(Raise);;
	eval empty_env exp;;

	(* BS-TLNIL *)
	let exp = Tl(Nil);;
	eval empty_env exp;;

	(* BS-TLCONS *)
	let exp = Tl(Cons(Num(1), Nil));;
	eval empty_env exp;;

	(* BS-TLRS *)
	let exp = Tl(Raise);;
	eval empty_env exp;;

	(* Testes Exceptions *)
	(* BS-RAISE *)
	let exp = Raise;;
	eval empty_env exp;;

	(* BS-TRY *)
	let exp = Try(Hd(Cons(Num(1), Nil)), Nil);;
	eval empty_env exp;;

	(* BS-TRYRS1 *)
	let exp = Try(Raise, Nil);;
	eval empty_env exp;;

	(* BS-TRYRS2 *)
	let exp = Try(Raise, Raise);;
	eval empty_env exp;;

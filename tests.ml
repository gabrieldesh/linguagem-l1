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

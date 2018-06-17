(* Este script pode ser usado no interpretador OCaml para carregar todos os 
   arquivos do projeto, usando o seguinte comando:
   
   #use "useAll.ml";;
*)

#mod_use "syntax.ml";;
#mod_use "typeConstraints.ml";;
#mod_use "collectTyEqs.ml";;
#mod_use "examples.ml";;
#mod_use "unify.ml";;

open Syntax;;
open TypeConstraints;;
open CollectTyEqs;;
open Examples;;
open Unify
open Syntax
open TypeConstraints
open Substituition

exception OccursCheckError of variable * tipo
exception UnificationError of typeConstraint

let unify eqs =
  let rec occurs x t = match t with
    | TyVar y             -> y == x
    | (TyInt | TyBool)    -> false
    | TyFn (t1, t2)       -> occurs x t1 || occurs x t2
    | TyList t            -> occurs x t
  in
  
  let rec replaceInType x tSub t = applySubstMapping (x, tSub) t
  in
  
  let rec replace x t constraints = match constraints with
    | [] -> []
    | (t1, t2) :: c -> (replaceInType x t t1,
                        replaceInType x t t2) :: (replace x t c)
  in
  
  let rec solve subst eqs = match eqs with
    | [] ->
        subst
    
    | (TyInt, TyInt) :: c ->
        solve subst c
    | (TyBool, TyBool) :: c ->
        solve subst c
    | (TyVar x, TyVar y) :: c when x == y ->
        solve subst c
    
    | (TyVar x, t) :: c ->
        if occurs x t
        then raise (OccursCheckError (x, t))
        else solve (subst @ [(x, t)]) (replace x t c)
    | (t, TyVar x) :: c ->
        if occurs x t
        then raise (OccursCheckError (x, t))
        else solve (subst @ [(x, t)]) (replace x t c)
    
    | (TyFn (t1, t2), TyFn (t3, t4)) :: c ->
        solve subst ((t3, t1) :: (t2, t4) :: c)
    | (TyList t1, TyList t2) :: c ->
        solve subst ((t1, t2) :: c)
    
    | (actualType, expectedType) :: c ->
        raise (UnificationError (actualType, expectedType))
  in
  solve [] eqs
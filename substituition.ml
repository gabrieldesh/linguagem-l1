open Syntax

type substMapping  = variable * tipo
type substituition = substMapping list

let rec applySubstMapping (x, tSub) t = match t with
  | TyVar y when y = x -> tSub
  | TyVar y            -> TyVar y
  | TyInt              -> TyInt
  | TyBool             -> TyBool
  | TyFn (t1, t2)      -> TyFn (applySubstMapping (x, tSub) t1,
                                applySubstMapping (x, tSub) t2)
  | TyList t           -> TyList (applySubstMapping (x, tSub) t)

let rec applySubs subs t = match subs with
  | []              -> t
  | mapping :: tail -> applySubs tail (applySubstMapping mapping t)
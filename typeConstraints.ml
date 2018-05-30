open Syntax

(* Um par (T1, T2) representa T1 = T2 *)
type typeConstraint = tipo * tipo

type typeConstraintSet = typeConstraint list
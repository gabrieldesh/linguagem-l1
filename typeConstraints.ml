open Syntax

(* Um par (T1, T2) representa T1 = T2, onde T1 é considerado o tipo inferido
(actual) e T2 o tipo esperado (expected). *)
type typeConstraint = tipo * tipo

type typeConstraintSet = typeConstraint list
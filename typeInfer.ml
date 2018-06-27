open CollectTyEqs
open Unify
open Substituition

let typeInfer env expr =
  let (t, c) = collectTyEqs env expr in
  let subs = unify c in
  applySubs subs t
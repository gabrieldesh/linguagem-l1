open Syntax
open TypeConstraints

type typeEnv = (variable * tipo) list

exception UndefinedIdentifier

(* Retorna o valor associado a uma chave em uma lista de pares (chave, valor). *)
let lookup = List.assoc
let newTypeVar varCount = (TyVar ("T" ^ string_of_int(varCount)), varCount + 1)

let rec collectTyEqs env expr varCount = match expr with
  (* C-Num *)
  | Num (_) ->
      (TyInt, [], varCount)
  
  (* C-Bool *)
  | Bool (_) ->
      (TyBool, [], varCount)
  
  (* C-<Op>, onde <Op> é uma operação Int x Int -> Int *)
  | Bop ((Sum | Diff | Mult | Div), e1, e2) ->
      let (t1, c1, varCount) = collectTyEqs env e1 varCount in
      let (t2, c2, varCount) = collectTyEqs env e2 varCount in
      (TyInt, c1 @ c2 @ [(t1, TyInt); (t2, TyInt)], varCount)
  
  (* C-Eq *)
  | Bop (Eq, e1, e2) ->
      let (t1, c1, varCount) = collectTyEqs env e1 varCount in
      let (t2, c2, varCount) = collectTyEqs env e2 varCount in
      (TyBool, c1 @ c2 @ [(t1, TyInt); (t2, TyInt)], varCount)
  
  (* C-<Op>, onde <Op> é uma operação Bool x Bool -> Bool *)
  | Bop ((And | Or), e1, e2) ->
      let (t1, c1, varCount) = collectTyEqs env e1 varCount in
      let (t2, c2, varCount) = collectTyEqs env e2 varCount in
      (TyBool, c1 @ c2 @ [(t1, TyBool); (t2, TyBool)], varCount)
  
  (* C-Not *)
  | Not e ->
      let (t, c, varCount) = collectTyEqs env e varCount in
      (TyBool, c @ [(t, TyBool)], varCount)
  
  (* C-If *)
  | If (e1, e2, e3) ->
      let (t1, c1, varCount) = collectTyEqs env e1 varCount in
      let (t2, c2, varCount) = collectTyEqs env e2 varCount in
      let (t3, c3, varCount) = collectTyEqs env e3 varCount in
      (t2, c1 @ c2 @ c3 @ [(t1, TyBool); (t2, t3)], varCount)
  
  (* C-Id *)
  | Var x ->
      (try let t = lookup x env in
           (t, [], varCount)
       with Not_found -> raise UndefinedIdentifier)
  
  (* C-App *)
  | App (e1, e2) ->
      let (t1, c1, varCount) = collectTyEqs env e1 varCount in
      let (t2, c2, varCount) = collectTyEqs env e2 varCount in
      let (x, varCount) = newTypeVar varCount in
      (x, c1 @ c2 @ [(t1, TyFn (t2, x))], varCount)
  
  (* C-Fn *)
  | Lam (x, t, e) ->
      let (t1, c, varCount) = collectTyEqs ((x, t)::env) e varCount in
      (TyFn (t, t1), c, varCount)
  
  (* C-FnImpl *)
  | LamImpl (id_x, e) ->
      let (type_x, varCount) = newTypeVar varCount in
      let (t, c, varCount) = collectTyEqs ((id_x, type_x)::env) e varCount in
      (TyFn (type_x, t), c, varCount)
  
  (* C-Let *)
  | Let (x, t, e1, e2) ->
      let (t1, c1, varCount) = collectTyEqs env e1 varCount in
      let (t2, c2, varCount) = collectTyEqs ((x, t)::env) e2 varCount in
      (t2, c1 @ c2 @ [(t, t1)], varCount)
  
  (* C-LetImpl *)
  | LetImpl (id_x, e1, e2) ->
      let (t1, c1, varCount) = collectTyEqs env e1 varCount in
      let (type_x, varCount) = newTypeVar varCount in
      let (t2, c2, varCount) = collectTyEqs ((id_x, type_x)::env) e2 varCount in
      (t2, c1 @ c2 @ [(type_x, t1)], varCount)
  
  (* C-LetR *)
  | Lrec (f, t', t, y, t'', e1, e2) ->
      let (t1, c1, varCount) = collectTyEqs ((f, TyFn (t', t))::(y, t')::env)
                                            e1 varCount in
      let (t2, c2, varCount) = collectTyEqs ((f, TyFn (t', t))::env)
                                            e2 varCount in
      (t2, c1 @ c2 @ [(t, t1); (t', t'')], varCount)
  
  (* C-LetRImpl *)
  | LrecImpl (f, id_y, e1, e2) ->
      let (type_x, varCount) = newTypeVar varCount in
      let (type_y, varCount) = newTypeVar varCount in
      let (t1, c1, varCount) = collectTyEqs ((f, type_x)::(id_y, type_y)::env)
                                            e1 varCount in
      let (t2, c2, varCount) = collectTyEqs ((f, type_x)::env) e2 varCount in
      (t2, c1 @ c2 @ [(type_x, TyFn (type_y, t1))], varCount)
  
  (* C-Nil *)
  | Nil ->
      let (x, varCount) = newTypeVar varCount in
      (TyList x, [], varCount)
  
  (* C-Cons *)
  | Cons (e1, e2) ->
      let (t1, c1, varCount) = collectTyEqs env e1 varCount in
      let (t2, c2, varCount) = collectTyEqs env e2 varCount in
      (t2, c1 @ c2 @ [(TyList t1, t2)], varCount)
  
  (* C-Hd *)
  | Hd e ->
      let (t, c, varCount) = collectTyEqs env e varCount in
      let (x, varCount) = newTypeVar varCount in
      (x, c @ [(t, TyList x)], varCount)
  
  (* C-Tl *)
  | Tl e ->
      let (t, c, varCount) = collectTyEqs env e varCount in
      let (x, varCount) = newTypeVar varCount in
      (TyList x, c @ [(t, TyList x)], varCount)
  
  (* C-IsEmpty *)
  | IsEmpty e ->
      let (t, c, varCount) = collectTyEqs env e varCount in
      let (x, varCount) = newTypeVar varCount in
      (TyBool, c @ [(t, TyList x)], varCount)
  
  (* C-Raise *)
  | Raise ->
      let (x, varCount) = newTypeVar varCount in
      (x, [], varCount)
  
  (* C-Try *)
  | Try (e1, e2) ->
      let (t1, c1, varCount) = collectTyEqs env e1 varCount in
      let (t2, c2, varCount) = collectTyEqs env e2 varCount in
      (t2, c1 @ c2 @ [(t1, t2)], varCount)
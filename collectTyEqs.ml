open Syntax
open TypeConstraints

type typeEnv = (variable * tipo) list

exception UndefinedIdentifier

(* Retorna o valor associado a uma chave em uma lista de pares (chave, valor) *)
let lookup = List.assoc

let collectTyEqs env expr =
  let varCount = ref 0 in
  
  let newTypeVar () =
    incr varCount;
    TyVar ("T" ^ string_of_int(!varCount))
  in

  let rec collect env expr = match expr with
    (* C-Num *)
    | Num (_) ->
        (TyInt, [])
    
    (* C-Bool *)
    | Bool (_) ->
        (TyBool, [])
    
    (* C-<Op>, onde <Op> é uma operação Int x Int -> Int *)
    | Bop ((Sum | Diff | Mult | Div), e1, e2) ->
        let (t1, c1) = collect env e1 in
        let (t2, c2) = collect env e2 in
        (TyInt, c1 @ c2 @ [(t1, TyInt); (t2, TyInt)])
    
    (* C-Eq *)
    | Bop (Eq, e1, e2) ->
        let (t1, c1) = collect env e1 in
        let (t2, c2) = collect env e2 in
        (TyBool, c1 @ c2 @ [(t1, TyInt); (t2, TyInt)])
    
    (* C-<Op>, onde <Op> é uma operação Bool x Bool -> Bool *)
    | Bop ((And | Or), e1, e2) ->
        let (t1, c1) = collect env e1 in
        let (t2, c2) = collect env e2 in
        (TyBool, c1 @ c2 @ [(t1, TyBool); (t2, TyBool)])
    
    (* C-Not *)
    | Not e ->
        let (t, c) = collect env e in
        (TyBool, c @ [(t, TyBool)])
    
    (* C-If *)
    | If (e1, e2, e3) ->
        let (t1, c1) = collect env e1 in
        let (t2, c2) = collect env e2 in
        let (t3, c3) = collect env e3 in
        (t2, c1 @ c2 @ c3 @ [(t1, TyBool); (t2, t3)])
    
    (* C-Id *)
    | Var x ->
        (try let t = lookup x env in
             (t, [])
         with Not_found -> raise UndefinedIdentifier)
    
    (* C-App *)
    | App (e1, e2) ->
        let (t1, c1) = collect env e1 in
        let (t2, c2) = collect env e2 in
        let x = newTypeVar () in
        (x, c1 @ c2 @ [(t1, TyFn (t2, x))])
    
    (* C-Fn *)
    | Lam (x, t, e) ->
        let (t1, c) = collect ((x, t)::env) e in
        (TyFn (t, t1), c)
    
    (* C-FnImpl *)
    | LamImpl (id_x, e) ->
        let type_x = newTypeVar () in
        let (t, c) = collect ((id_x, type_x)::env) e in
        (TyFn (type_x, t), c)
    
    (* C-Let *)
    | Let (x, t, e1, e2) ->
        let (t1, c1) = collect env e1 in
        let (t2, c2) = collect ((x, t)::env) e2 in
        (t2, c1 @ c2 @ [(t, t1)])
    
    (* C-LetImpl *)
    | LetImpl (id_x, e1, e2) ->
        let (t1, c1) = collect env e1 in
        let type_x = newTypeVar () in
        let (t2, c2) = collect ((id_x, type_x)::env) e2 in
        (t2, c1 @ c2 @ [(type_x, t1)])
    
    (* C-LetR *)
    | Lrec (f, t', t, y, t'', e1, e2) ->
        let (t1, c1) = collect ((f, TyFn (t', t))::(y, t')::env) e1 in
        let (t2, c2) = collect ((f, TyFn (t', t))::env) e2 in
        (t2, c1 @ c2 @ [(t, t1); (t', t'')])
    
    (* C-LetRImpl *)
    | LrecImpl (f, id_y, e1, e2) ->
        let type_x = newTypeVar () in
        let type_y = newTypeVar () in
        let (t1, c1) = collect ((f, type_x)::(id_y, type_y)::env) e1 in
        let (t2, c2) = collect ((f, type_x)::env) e2 in
        (t2, c1 @ c2 @ [(type_x, TyFn (type_y, t1))])
    
    (* C-Nil *)
    | Nil ->
        let x = newTypeVar () in
        (TyList x, [])
    
    (* C-Cons *)
    | Cons (e1, e2) ->
        let (t1, c1) = collect env e1 in
        let (t2, c2) = collect env e2 in
        (t2, c1 @ c2 @ [(TyList t1, t2)])
    
    (* C-Hd *)
    | Hd e ->
        let (t, c) = collect env e in
        let x = newTypeVar () in
        (x, c @ [(t, TyList x)])
    
    (* C-Tl *)
    | Tl e ->
        let (t, c) = collect env e in
        let x = newTypeVar () in
        (TyList x, c @ [(t, TyList x)])
    
    (* C-IsEmpty *)
    | IsEmpty e ->
        let (t, c) = collect env e in
        let x = newTypeVar () in
        (TyBool, c @ [(t, TyList x)])
    
    (* C-Raise *)
    | Raise ->
        let x = newTypeVar () in
        (x, [])
    
    (* C-Try *)
    | Try (e1, e2) ->
        let (t1, c1) = collect env e1 in
        let (t2, c2) = collect env e2 in
        (t2, c1 @ c2 @ [(t1, t2)])
  in
  collect env expr
open Syntax

(*
let rec fat : int -> int = (fn n : int =>
  if n = 0
  then 1
  else n * fat (n - 1)
) in
fat
*)
let progFat =
  Lrec ("fat", TyInt, TyInt, "n", TyInt, 
        If (Bop (Eq, Var "n", Num 0),
            Num 1,
            Bop (Mult, Var "n", App (Var "fat", Bop (Diff, Var "n", Num 1)))),
        Var "fat")

(* Mesmo programa, mas com tipagem implícita. *)
let progFatImpl =
  LrecImpl ("fat", "n",
            If (Bop (Eq, Var "n", Num 0),
                Num 1,
                Bop (Mult, Var "n", App (Var "fat", Bop (Diff, Var "n", Num 1)))),
            Var "fat")

(*
let rec map : (int -> int) -> int list -> int list = (fn f : (int -> int) =>
  (fn l : int list =>
    if not (isempty l)
    then (f (hd l)) :: (tl l)
    else nil
  )
) in
let inc : int -> int = (fn n : int => n + 1) in
map inc (0 :: 1 :: nil)
*)
let progMap =
  Lrec ("map", TyFn (TyInt, TyInt), TyFn (TyList TyInt, TyList TyInt),
        "f", TyFn (TyInt, TyInt),
        Lam ("l", TyList TyInt,
             If (Not (IsEmpty (Var "l")),
                 Cons (App (Var "f", Hd (Var "l")), Tl (Var "l")),
                 Nil)),
        Let ("inc", TyFn (TyInt, TyInt),
             Lam ("n", TyInt, Bop (Sum, Var "n", Num 1)),
             App (App (Var "map", Var "inc"), Cons(Num 0, Cons(Num 1, Nil)))))

(* Mesmo programa, mas com tipagem implícita. *)
 let progMapImpl =
   LrecImpl ("map", "f",
             LamImpl ("l",
                      If (Not (IsEmpty (Var "l")),
                          Cons (App (Var "f", Hd (Var "l")), Tl (Var "l")),
                          Nil)),
             LetImpl ("inc",
                      LamImpl ("n", Bop (Sum, Var "n", Num 1)),
                      App (App (Var "map", Var "inc"), Cons(Num 0, Cons(Num 1, Nil)))))

(*
let f = (fn l =>
  1 / (hd l)
) in
try f (0 :: nil)
with try f nil
     with try f (2 :: nil)
          with raise
*)
let progTry =
  LetImpl ("f", LamImpl ("l",
                         Bop (Div, Num 1, Hd (Var "l"))),
           Try (App (Var "f", Cons (Num 0, Nil)),
                Try (App (Var "f", Nil),
                     Try (App (Var "f", Cons (Num 2, Nil)),
                          Raise))))

(*
let rec f = (fn x => f) in f
*)
let progInfiniteType =
  LrecImpl ("f", "x", Var "f", Var "f")

(*
fn f => (fn x => f (x x)) (fn x => f (x x))
*)
let progYCombinator =
  LamImpl("f", App (LamImpl ("x", App (Var "f", App (Var "x", Var "x"))),
                    LamImpl ("x", App (Var "f", App (Var "x", Var "x")))))
open Ast 
open Util

(* 
(add1 (add1 (sub1 6)))   
*)

let rec fold : expr -> expr = function 
  | Num n -> Num n
  | Prim1 (Add1, e) -> (
    match fold e with 
    | Num n -> Num (n+1)
    | e -> Prim1 (Add1, e))
  | Prim1 (Sub1, e) -> (
    match fold e with 
    | Num n -> Num (n-1)
    | e -> Prim1 (Sub1, e))
  | Prim1 (p, e) -> Prim1 (p, fold e) 
  | Prim2 (Plus, e1, e2) -> (
    match fold e1, fold e2 with 
    | Num x, Num y -> Num (x + y)
    | e1, e2 -> Prim2 (Plus, e1, e2)) 
  | Prim2 (Minus, e1, e2) -> (
    match fold e1, fold e2 with 
    | Num x, Num y -> Num (x - y)
    | e1, e2 -> Prim2 (Minus, e1, e2))
  | Prim2 (p, e1, e2) -> Prim2 (p, fold e1, fold e2)
  | If (e1, e2, e3) ->
    If (fold e1, fold e2, fold e3)
  | Let (v, e, b) ->
    Let (v, fold e, fold b)
  | Call (e, args) ->
    Call (fold e, List.map fold args)
  | Do exps ->
    Do (List.map fold exps)
  | e -> e 

let fold_program (prog : program) =
  { defns=
      List.map
        (fun {name; args; body} -> {name; args; body= fold body})
        prog.defns
  ; body= fold prog.body }
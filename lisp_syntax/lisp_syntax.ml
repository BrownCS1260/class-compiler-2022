open S_exp
open Ast

exception ParseError of s_exp

let prim1_of_string = function
  | "add1" ->
      Some Add1
  | "sub1" ->
      Some Sub1
  | "zero?" ->
      Some IsZero
  | "num?" ->
      Some IsNum
  | "not" ->
      Some Not
  | _ ->
      None

let prim2_of_string = function
  | "+" ->
      Some Plus
  | "-" ->
      Some Minus
  | "=" ->
      Some Eq
  | "<" ->
      Some Lt
  | _ ->
      None

let rec expr_of_s_exp : s_exp -> expr = function
  | Num x ->
      Num x
  | Sym "true" ->
      True
  | Sym "false" ->
      False
  | Lst [Sym "if"; test_s; then_s; else_s] ->
      If (expr_of_s_exp test_s, expr_of_s_exp then_s, expr_of_s_exp else_s)
  | Lst [Sym prim; arg] when Option.is_some (prim1_of_string prim) ->
      Prim1 (Option.get (prim1_of_string prim), expr_of_s_exp arg)
  | Lst [Sym prim; arg1; arg2] when Option.is_some (prim2_of_string prim) ->
      Prim2
        ( Option.get (prim2_of_string prim)
        , expr_of_s_exp arg1
        , expr_of_s_exp arg2 )
  | e ->
      raise (ParseError e)

let parse (s : string) = S_exp.parse s |> expr_of_s_exp

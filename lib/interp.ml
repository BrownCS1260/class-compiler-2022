(* open S_exp *)

open Ast
(*//TODO: open Util?*)

type value = Number of int | Boolean of bool

let string_of_val (v : value) : string =
  match v with Number n -> string_of_int n | Boolean b -> string_of_bool b

exception BadExpression of s_exp

let interp_unary_primitive prim arg = 
  match (prim, arg) with
  | Add1, Num x ->
      Some (Num (x + 1))
  | Sub1, Num x ->
      Some (Num (x - 1))
  | IsZero, Num 0 ->
      Some (Bool true)
  | IsZero, _ ->
      Some (Bool false)
  | IsNum, Num _ ->
      Some (Bool true)
  | IsNum, _ ->
      Some (Bool false)
  | Not, Bool false ->
      Some (Bool true)
  | Not, _ ->
      Some (Bool false)
  | _ ->
      None

let interp_binary_primitive prim arg1 arg2 = 
  match (prim, arg1, arg2) with
  | Plus, Num x1, Num x2 ->
      Some (Num (x1 + x2))
  | Minus, Num x1, Num x2 ->
      Some (Num (x1 - x2))
  | Eq, Num x1, Num x2 ->
      Some (Bool (x1 = x2))
  | Lt, Num x1, Num x2 ->
      Some (Bool (x1 < x2))
  | _ ->
      None

let rec interp_expr : expr -> value = 
  function
  | Num x ->
      Num x
  | Nil ->
      Nil
  | If (test_exp, then_exp, else_exp) ->
    if interp_expr defns env test_exp <> Bool false then
      interp_expr defns env then_exp
    else interp_expr defns env else_exp
  | Prim1 (f, arg) as e -> (
    match interp_unary_primitive f (interp_expr arg) with
    | Some v ->
        v
    | None ->
        raise (Error.Stuck e) )
  | Prim2 (f, arg1, arg2) as e -> (
    match
      let v1 = interp_expr defns env arg1 in
      let v2 = interp_expr defns env arg2 in
      interp_binary_primitive f v1 v2
    with
    | Some v ->
        v
    | None ->
        raise (Error.Stuck e) )
  | True ->
      Bool true
  | False ->
      Bool false

(* let rec interp_exp (exp : s_exp) : value =
  match exp with
  | Num n -> Number n
  | Sym "true" -> Boolean true
  | Sym "false" -> Boolean false
  | Lst [ Sym "add1"; arg ] -> (
      match interp_exp arg with
      | Number n -> Number (n + 1)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "sub1"; arg ] -> (
      match interp_exp arg with
      | Number n -> Number (n - 1)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "not"; arg ] ->
      if interp_exp arg = Boolean false then Boolean true else Boolean false
  | Lst [ Sym "num?"; arg ] -> (
      match interp_exp arg with Number _ -> Boolean true | _ -> Boolean false)
  | Lst [ Sym "zero?"; arg ] ->
      if interp_exp arg = Number 0 then Boolean true else Boolean false
  | Lst [ Sym "if"; test_exp; then_exp; else_exp ] ->
      if interp_exp test_exp = Boolean false then interp_exp else_exp
      else interp_exp then_exp
  | Lst [ Sym "+"; e1; e2 ] -> (
      match (interp_exp e1, interp_exp e2) with
      | Number n1, Number n2 -> Number (n1 + n2)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "-"; e1; e2 ] -> (
      match (interp_exp e1, interp_exp e2) with
      | Number n1, Number n2 -> Number (n1 - n2)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "="; e1; e2 ] -> Boolean (interp_exp e1 = interp_exp e2)
  | Lst [ Sym "<"; e1; e2 ] -> (
      match (interp_exp e1, interp_exp e2) with
      | Number n1, Number n2 -> Boolean (n1 < n2)
      | _ -> raise (BadExpression exp))
  | _ -> raise (BadExpression exp) *)

let interp (program : string) : string =
  parse program |> interp_expr |> string_of_expr

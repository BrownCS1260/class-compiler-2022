(* open S_exp *)
(* open Lisp_syntax *)

open Ast
(*//TODO: open Util?*)

type value = Num of int | Bool of bool

let string_of_val (v : value) : string =
  match v with Num n -> string_of_int n | Bool b -> string_of_bool b

exception BadExpression of expr

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
  | If (test_exp, then_exp, else_exp) ->
    if interp_expr test_exp <> Bool false then
      interp_expr then_exp
    else interp_expr else_exp
  | Prim1 (f, arg) as e -> (
    match interp_unary_primitive f (interp_expr arg) with
    | Some v ->
        v
    | None ->
        raise (BadExpression e) )
  | Prim2 (f, arg1, arg2) as e -> (
    match
      let v1 = interp_expr arg1 in
      let v2 = interp_expr arg2 in
      interp_binary_primitive f v1 v2
    with
    | Some v ->
        v
    | None ->
        raise (BadExpression e) )
  | True ->
      Bool true
  | False ->
      Bool false

let interp (program : string) =
  Lisp_syntax.parse program |> interp_expr |> ignore

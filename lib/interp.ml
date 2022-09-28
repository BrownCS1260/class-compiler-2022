open S_exp
open Util

type value = Number of int | Boolean of bool

let string_of_val (v : value) : string =
  match v with Number n -> string_of_int n | Boolean b -> string_of_bool b

exception BadExpression of s_exp

let rec interp_exp (env : value symtab) (exp : s_exp) : value =
  match exp with
  | Sym var when Symtab.mem var env -> Symtab.find var env
  | Num n -> Number n
  | Sym "true" -> Boolean true
  | Sym "false" -> Boolean false
  | Lst [ Sym "add1"; arg ] -> (
      match interp_exp env arg with
      | Number n -> Number (n + 1)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "sub1"; arg ] -> (
      match interp_exp env arg with
      | Number n -> Number (n - 1)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "not"; arg ] ->
      if interp_exp env arg = Boolean false then Boolean true else Boolean false
  | Lst [ Sym "num?"; arg ] -> (
      match interp_exp env arg with Number _ -> Boolean true | _ -> Boolean false)
  | Lst [ Sym "zero?"; arg ] ->
      if interp_exp env arg = Number 0 then Boolean true else Boolean false
  | Lst [ Sym "if"; test_exp; then_exp; else_exp ] ->
      if interp_exp env test_exp = Boolean false then interp_exp env else_exp
      else interp_exp env then_exp
  | Lst [ Sym "+"; e1; e2 ] -> (
      match (interp_exp env e1, interp_exp env e2) with
      | Number n1, Number n2 -> Number (n1 + n2)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "-"; e1; e2 ] -> (
      match (interp_exp env e1, interp_exp env e2) with
      | Number n1, Number n2 -> Number (n1 - n2)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "="; e1; e2 ] -> Boolean (interp_exp env e1 = interp_exp env e2)
  | Lst [ Sym "<"; e1; e2 ] -> (
      match (interp_exp env e1, interp_exp env e2) with
      | Number n1, Number n2 -> Boolean (n1 < n2)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; e ]]; body ] -> 
      let e_value = interp_exp env e in 
      interp_exp (Symtab.add var e_value env) body
  | _ -> raise (BadExpression exp)

let interp (program : string) : string =
  parse program |> interp_exp Symtab.empty |> string_of_val

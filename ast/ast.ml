open S_exp

type prim1 =
  | Add1
  | Sub1
  | IsZero
  | IsNum
  | Not

type prim2 = Plus | Minus | Eq | Lt

type expr =
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
  | If of expr * expr * expr
  | Num of int
  | Var of string
  | True
  | False
  | Nil

type defn = {name: string; args: string list; body: expr}

type program = {defns: defn list; body: expr}

let is_defn defns name = List.exists (fun d -> d.name = name) defns

let get_defn defns name = List.find (fun d -> d.name = name) defns

let rec s_exp_of_expr = function
  | Prim1 (Add1, arg) ->
      Lst [Sym "add1"; s_exp_of_expr arg]
  | Prim1 (Sub1, arg) ->
      Lst [Sym "sub1"; s_exp_of_expr arg]
  | Prim1 (IsZero, arg) ->
      Lst [Sym "zero?"; s_exp_of_expr arg]
  | Prim1 (IsNum, arg) ->
      Lst [Sym "num?"; s_exp_of_expr arg]
  | Prim1 (Not, arg) ->
      Lst [Sym "not"; s_exp_of_expr arg]
  | Prim2 (Plus, arg1, arg2) ->
      Lst [Sym "+"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Minus, arg1, arg2) ->
      Lst [Sym "-"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Lt, arg1, arg2) ->
      Lst [Sym "<"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Eq, arg1, arg2) ->
      Lst [Sym "="; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | If (e1, e2, e3) ->
      Lst [Sym "if"; s_exp_of_expr e1; s_exp_of_expr e2; s_exp_of_expr e3]
  | True ->
      Sym "true"
  | False ->
      Sym "false"
  | Nil ->
      Lst []
  | Var x ->
      Sym x
  | Num n ->
      Num n

let string_of_expr s = s |> s_exp_of_expr |> string_of_s_exp

open S_exp
open Util

type value = Number of int | Boolean of bool | Pair of (value * value)

let input_channel = ref stdin
let output_channel = ref stdout

let rec string_of_val (v : value) : string =
  match v with
  | Number n -> string_of_int n
  | Boolean b -> string_of_bool b
  | Pair (v1, v2) ->
      Printf.sprintf "(pair %s %s)" (string_of_val v1) (string_of_val v2)

exception BadExpression of s_exp

let rec interp_exp (defns : defn list) (env : value symtab) (exp : s_exp) : value =
  match exp with
  | Lst (Sym f :: args) when is_defn defns f -> 
    let defn = get_defn defns f in 
    if List.length args <> List.length defn.args then raise (BadExpression exp) else 
        let vals = List.map (interp_exp defns env) args in 
        let fenv = List.fold_left (fun t (name, v) -> Symtab.add name v t) 
          Symtab.empty (List.combine defn.args vals) in 
        interp_exp defns fenv defn.body
  | Sym var when Symtab.mem var env -> Symtab.find var env
  | Num n -> Number n
  | Sym "true" -> Boolean true
  | Sym "false" -> Boolean false
  | Lst [ Sym "add1"; arg ] -> (
      match interp_exp defns env arg with
      | Number n -> Number (n + 1)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "sub1"; arg ] -> (
      match interp_exp defns env arg with
      | Number n -> Number (n - 1)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "not"; arg ] ->
      if interp_exp defns env arg = Boolean false then Boolean true else Boolean false
  | Lst [ Sym "num?"; arg ] -> (
      match interp_exp defns env arg with
      | Number _ -> Boolean true
      | _ -> Boolean false)
  | Lst [ Sym "zero?"; arg ] ->
      if interp_exp defns env arg = Number 0 then Boolean true else Boolean false
  | Lst [ Sym "if"; test_exp; then_exp; else_exp ] ->
      if interp_exp defns env test_exp = Boolean false then interp_exp defns env else_exp
      else interp_exp defns env then_exp
  | Lst [ Sym "+"; e1; e2 ] -> (
      let l = interp_exp defns env e1 in
      let r = interp_exp defns env e2 in
      match (l, r) with
      | Number n1, Number n2 -> Number (n1 + n2)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "-"; e1; e2 ] -> (
      let l = interp_exp defns env e1 in
      let r = interp_exp defns env e2 in
      match (l, r) with
      | Number n1, Number n2 -> Number (n1 - n2)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "="; e1; e2 ] -> (
      let l = interp_exp defns env e1 in
      let r = interp_exp defns env e2 in
      match (l, r) with
      | Number n1, Number n2 -> Boolean (n1 = n2)
      | Boolean b1, Boolean b2 -> Boolean (b1 = b2)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "<"; e1; e2 ] -> (
      let l = interp_exp defns env e1 in
      let r = interp_exp defns env e2 in
      match (l, r) with
      | Number n1, Number n2 -> Boolean (n1 < n2)
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; e ] ]; body ] ->
      let e_value = interp_exp defns env e in
      interp_exp defns (Symtab.add var e_value env) body
  | Lst [ Sym "pair"; e1; e2 ] ->
      let left = interp_exp defns env e1 in
      let right = interp_exp defns env e2 in
      Pair (left, right)
  | Lst [ Sym "left"; e1 ] -> (
      match interp_exp defns env e1 with
      | Pair (v1, _) -> v1
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "right"; e1 ] -> (
      match interp_exp defns env e1 with
      | Pair (_, v2) -> v2
      | _ -> raise (BadExpression exp))
  | Lst [ Sym "read-num" ] -> Number (input_line !input_channel |> int_of_string)
  | Lst [ Sym "print"; e ] -> 
    interp_exp defns env e |> string_of_val |> output_string !output_channel;
    Boolean true
  | Lst [ Sym "newline" ] -> output_string !output_channel "\n";
    Boolean true
  | Lst (Sym "do" :: exps) when List.length exps > 0 -> 
    exps |> List.rev_map (interp_exp defns env) |> List.hd
  | _ -> raise (BadExpression exp)

  let interp (program : string) : unit =
    let defns, body = parse_many program |> defns_and_body in
    interp_exp defns Symtab.empty body |> ignore
  
  let interp_io (program : string) (input : string) =
    let input_pipe_ex, input_pipe_en = Unix.pipe () in
    let output_pipe_ex, output_pipe_en = Unix.pipe () in
    input_channel := Unix.in_channel_of_descr input_pipe_ex ;
    set_binary_mode_in !input_channel false ;
    output_channel := Unix.out_channel_of_descr output_pipe_en ;
    set_binary_mode_out !output_channel false ;
    let write_input_channel = Unix.out_channel_of_descr input_pipe_en in
    set_binary_mode_out write_input_channel false ;
    let read_output_channel = Unix.in_channel_of_descr output_pipe_ex in
    set_binary_mode_in read_output_channel false ;
    output_string write_input_channel input ;
    close_out write_input_channel ;
    interp program ;
    close_out !output_channel ;
    let r = input_all read_output_channel in
    input_channel := stdin ;
    output_channel := stdout ;
    r
  
  let interp_err (program : string) (input : string) : string =
    try interp_io program input with BadExpression _ -> "ERROR"
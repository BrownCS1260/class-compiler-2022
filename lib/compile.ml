(* open S_exp *)
open Asm
open Ast
(* open Util *)

exception BadExpression of s_exp

let num_shift = 2
let num_mask = 0b11
let num_tag = 0b00
let bool_shift = 7
let bool_mask = 0b1111111
let bool_tag = 0b0011111

let operand_of_bool (b : bool) : operand =
  Imm (((if b then 1 else 0) lsl bool_shift) lor bool_tag)

let operand_of_num (x : int) : operand = Imm ((x lsl num_shift) lor num_tag)

let zf_to_bool : directive list =
  [
    Mov (Reg Rax, Imm 0);
    Setz (Reg Rax);
    Shl (Reg Rax, Imm bool_shift);
    Or (Reg Rax, Imm bool_tag);
  ]

let lf_to_bool : directive list =
  [
    Mov (Reg Rax, Imm 0);
    Setl (Reg Rax);
    Shl (Reg Rax, Imm bool_shift);
    Or (Reg Rax, Imm bool_tag);
  ]

let stack_address (index : int) : operand = MemOffset (Imm index, Reg Rsp)

let compile_unary_primitive e = function
| Add1 ->
  [Add (Reg Rax, operand_of_num 1)]
| Sub1 ->
  [Sub (Reg Rax, operand_of_num 1)]
| IsZero ->
  [Cmp (Reg Rax, operand_of_num 0)] @ zf_to_bool
| IsNum ->
  [And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag)] @ zf_to_bool
| Not ->
  [Cmp (Reg Rax, operand_of_bool false)] @ zf_to_bool

let compile_binary_primitive stack_index e = function
  | Plus ->
      [Add (Reg Rax, stack_address stack_index)]
  | Minus ->
      [ Mov (Reg R8, Reg Rax)
        ; Mov (Reg Rax, stack_address stack_index)
        ; Sub (Reg Rax, Reg R8) ]
  | Eq ->
      [Cmp (stack_address stack_index, Reg Rax)]
      @ zf_to_bool
  | Lt ->
      [Cmp (stack_address stack_index, Reg Rax)]
      @ lf_to_bool

let rec compile_expr (stack_index : int) : expr -> directive list = function
| Num x ->
    [Mov (Reg Rax, operand_of_num x)]
| True ->
    [Mov (Reg Rax, operand_of_bool true)]
| False ->
    [Mov (Reg Rax, operand_of_bool false)]
| Nil ->
    [Mov (Reg Rax, operand_of_nil)]
| If (test_expr, then_expr, else_expr) ->
    let then_label = gensym "then" in
    let else_label = gensym "else" in
    let continue_label = gensym "continue" in
    compile_expr stack_index test_expr
    @ [Cmp (Reg Rax, operand_of_bool false); Je else_label]
    @ [Label then_label]
    @ compile_expr stack_index then_expr
    @ [Jmp continue_label] @ [Label else_label]
    @ compile_expr stack_index else_expr
    @ [Label continue_label]
| Prim1 (f, arg) as exp ->
    compile_expr stack_index arg
    @ compile_unary_primitive exp f
| Prim2 (f, arg1, arg2) as exp ->
    compile_expr stack_index arg1
    @ [Mov (stack_address stack_index, Reg Rax)]
    @ compile_expr (stack_index - 8) arg2
    @ compile_binary_primitive stack_index exp f

(* let rec compile_exp (stack_index : int) (exp : s_exp) : directive list =
  match exp with
  | Num n -> [ Mov (Reg Rax, operand_of_num n) ]
  | Sym "true" -> [ Mov (Reg Rax, operand_of_bool true) ]
  | Sym "false" -> [ Mov (Reg Rax, operand_of_bool false) ]
  | Lst [ Sym "not"; arg ] ->
      compile_exp stack_index arg
      @ [ Cmp (Reg Rax, operand_of_bool false) ]
      @ zf_to_bool
  | Lst [ Sym "zero?"; arg ] ->
      compile_exp stack_index arg
      @ [ Cmp (Reg Rax, operand_of_num 0) ]
      @ zf_to_bool
  | Lst [ Sym "num?"; arg ] ->
      compile_exp stack_index arg
      @ [ And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag) ]
      @ zf_to_bool
  | Lst [ Sym "add1"; arg ] ->
      compile_exp stack_index arg @ [ Add (Reg Rax, operand_of_num 1) ]
  | Lst [ Sym "sub1"; arg ] ->
      compile_exp stack_index arg @ [ Sub (Reg Rax, operand_of_num 1) ]
  | Lst [ Sym "if"; test_exp; then_exp; else_exp ] ->
      let else_label = Util.gensym "else" in
      let continue_label = Util.gensym "continue" in
      compile_exp stack_index test_exp
      @ [ Cmp (Reg Rax, operand_of_bool false); Jz else_label ]
      @ compile_exp stack_index then_exp
      @ [ Jmp continue_label ] @ [ Label else_label ]
      @ compile_exp stack_index else_exp
      @ [ Label continue_label ]
  | Lst [ Sym "+"; e1; e2 ] ->
      compile_exp stack_index e1
      @ [ Mov (MemOffset (Reg Rsp, Imm stack_index), Reg Rax) ]
      @ compile_exp (stack_index - 8) e2
      @ [ Mov (Reg R8, MemOffset (Reg Rsp, Imm stack_index)) ]
      @ [ Add (Reg Rax, Reg R8) ]
  | Lst [ Sym "-"; e1; e2 ] ->
      compile_exp stack_index e1
      @ [ Mov (MemOffset (Reg Rsp, Imm stack_index), Reg Rax) ]
      @ compile_exp (stack_index - 8) e2
      @ [ Mov (Reg R8, MemOffset (Reg Rsp, Imm stack_index)) ]
      @ [ Sub (Reg Rax, Reg R8) ]
  | Lst [ Sym "<"; e1; e2 ] ->
      compile_exp stack_index e1
      @ [ Mov (MemOffset (Reg Rsp, Imm stack_index), Reg Rax) ]
      @ compile_exp (stack_index - 8) e2
      @ [ Mov (Reg R8, MemOffset (Reg Rsp, Imm stack_index)) ]
      @ [ Cmp (Reg R8, Reg Rax) ]
      @ lf_to_bool
  | Lst [ Sym "="; e1; e2 ] ->
      compile_exp stack_index e1
      @ [ Mov (MemOffset (Reg Rsp, Imm stack_index), Reg Rax) ]
      @ compile_exp (stack_index - 8) e2
      @ [ Mov (Reg R8, MemOffset (Reg Rsp, Imm stack_index)) ]
      @ [ Cmp (Reg R8, Reg Rax) ]
      @ zf_to_bool
  | _ -> raise (BadExpression exp) *)

let compile (program : expr) : string =
  [ Global "entry"; Label "entry" ] @ compile_expr (-8) program @ [ Ret ]
  |> List.map string_of_directive
  |> String.concat "\n"

let compile_to_file (program : string) : unit =
  let file = open_out "program.s" in
  output_string file (compile (parse program));
  close_out file

let compile_and_run (program : string) : string =
  compile_to_file program;
  ignore (Unix.system "nasm program.s -f elf64 -o program.o");
  ignore (Unix.system "gcc program.o runtime.o -o program");
  let inp = Unix.open_process_in "./program" in
  let r = input_line inp in
  close_in inp;
  r

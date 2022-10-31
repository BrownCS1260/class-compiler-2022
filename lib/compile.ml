open S_exp
open Asm
open Util

let num_shift = 2
let num_mask = 0b11
let num_tag = 0b00
let bool_shift = 7
let bool_mask = 0b1111111
let bool_tag = 0b0011111
let heap_mask = 0b111
let pair_tag = 0b010

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

(* modifies R8 *)
let ensure_num (op : operand) : directive list =
  [
    Mov (Reg R8, op);
    And (Reg R8, Imm num_mask);
    Cmp (Reg R8, Imm num_tag);
    Jnz "error";
  ]

(* modifies R8 *)
let ensure_pair (op : operand) : directive list =
  [
    Mov (Reg R8, op);
    And (Reg R8, Imm heap_mask);
    Cmp (Reg R8, Imm pair_tag);
    Jnz "error";
  ]

let stack_address stack_index = MemOffset (Reg Rsp, Imm stack_index)

let align_stack_index (stack_index : int) : int =
  if stack_index mod 16 = -8 then stack_index else stack_index - 8

let rec compile_exp (defns : defn list) (tab : int symtab) (stack_index : int)
    (exp : s_exp) (is_tail : bool) : directive list =
  match exp with
  | Lst (Sym f :: args) when is_defn defns f && is_tail ->
      let defn = get_defn defns f in
      if List.length args <> List.length defn.args then
        raise (BadExpression exp)
      else
        let compiled_args =
          args
          |> List.mapi (fun i arg ->
                 compile_exp defns tab (stack_index - (8 * i)) arg false
                 @ [ Mov (stack_address (stack_index - (8 * i)), Reg Rax) ])
          |> List.concat
        in
        let moved_args =
          args
          |> List.mapi (fun i _ ->
                 [
                   Mov (Reg R8, stack_address (stack_index - (8 * i)));
                   Mov (stack_address ((i + 1) * -8), Reg R8);
                 ])
          |> List.concat
        in
        compiled_args @ moved_args @ [ Jmp (defn_label f) ]
  | Lst (Sym f :: args) when is_defn defns f ->
      let defn = get_defn defns f in
      if List.length args <> List.length defn.args then
        raise (BadExpression exp)
      else
        let stack_base = align_stack_index (stack_index + 8) in
        let compiled_args =
          args
          |> List.mapi (fun i arg ->
                 compile_exp defns tab (stack_base - (8 * (i + 2))) arg false
                 @ [ Mov (stack_address (stack_base - (8 * (i + 2))), Reg Rax) ])
          |> List.concat
        in
        compiled_args
        @ [
            Add (Reg Rsp, Imm stack_base);
            Call (defn_label f);
            Sub (Reg Rsp, Imm stack_base);
          ]
  | Sym var when Symtab.mem var tab ->
      [ Mov (Reg Rax, stack_address (Symtab.find var tab)) ]
  | Num n -> [ Mov (Reg Rax, operand_of_num n) ]
  | Sym "true" -> [ Mov (Reg Rax, operand_of_bool true) ]
  | Sym "false" -> [ Mov (Reg Rax, operand_of_bool false) ]
  | Lst [ Sym "not"; arg ] ->
      compile_exp defns tab stack_index arg false
      @ [ Cmp (Reg Rax, operand_of_bool false) ]
      @ zf_to_bool
  | Lst [ Sym "zero?"; arg ] ->
      compile_exp defns tab stack_index arg false
      @ [ Cmp (Reg Rax, operand_of_num 0) ]
      @ zf_to_bool
  | Lst [ Sym "num?"; arg ] ->
      compile_exp defns tab stack_index arg false
      @ [ And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag) ]
      @ zf_to_bool
  | Lst [ Sym "add1"; arg ] ->
      compile_exp defns tab stack_index arg false
      @ ensure_num (Reg Rax)
      @ [ Add (Reg Rax, operand_of_num 1) ]
  | Lst [ Sym "sub1"; arg ] ->
      compile_exp defns tab stack_index arg false
      @ ensure_num (Reg Rax)
      @ [ Sub (Reg Rax, operand_of_num 1) ]
  | Lst [ Sym "if"; test_exp; then_exp; else_exp ] ->
      let else_label = Util.gensym "else" in
      let continue_label = Util.gensym "continue" in
      compile_exp defns tab stack_index test_exp false
      @ [ Cmp (Reg Rax, operand_of_bool false); Jz else_label ]
      @ compile_exp defns tab stack_index then_exp is_tail
      @ [ Jmp continue_label ] @ [ Label else_label ]
      @ compile_exp defns tab stack_index else_exp is_tail
      @ [ Label continue_label ]
  | Lst [ Sym "+"; e1; e2 ] ->
      compile_exp defns tab stack_index e1 false
      @ ensure_num (Reg Rax)
      @ [ Mov (stack_address stack_index, Reg Rax) ]
      @ compile_exp defns tab (stack_index - 8) e2 false
      @ ensure_num (Reg Rax)
      @ [ Mov (Reg R8, stack_address stack_index) ]
      @ [ Add (Reg Rax, Reg R8) ]
  | Lst [ Sym "-"; e1; e2 ] ->
      compile_exp defns tab stack_index e1 false
      @ ensure_num (Reg Rax)
      @ [ Mov (stack_address stack_index, Reg Rax) ]
      @ compile_exp defns tab (stack_index - 8) e2 false
      @ ensure_num (Reg Rax)
      @ [
          Mov (Reg R8, Reg Rax);
          Mov (Reg Rax, stack_address stack_index);
          Sub (Reg Rax, Reg R8);
        ]
  | Lst [ Sym "<"; e1; e2 ] ->
      compile_exp defns tab stack_index e1 false
      @ ensure_num (Reg Rax)
      @ [ Mov (stack_address stack_index, Reg Rax) ]
      @ compile_exp defns tab (stack_index - 8) e2 false
      @ ensure_num (Reg Rax)
      @ [ Mov (Reg R8, stack_address stack_index) ]
      @ [ Cmp (Reg R8, Reg Rax) ]
      @ lf_to_bool
  | Lst [ Sym "="; e1; e2 ] ->
      compile_exp defns tab stack_index e1 false
      @ ensure_num (Reg Rax)
      @ [ Mov (stack_address stack_index, Reg Rax) ]
      @ compile_exp defns tab (stack_index - 8) e2 false
      @ ensure_num (Reg Rax)
      @ [ Mov (Reg R8, stack_address stack_index) ]
      @ [ Cmp (Reg R8, Reg Rax) ]
      @ zf_to_bool
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; e ] ]; body ] ->
      compile_exp defns tab stack_index e false
      @ [ Mov (stack_address stack_index, Reg Rax) ]
      @ compile_exp defns
          (Symtab.add var stack_index tab)
          (stack_index - 8) body is_tail
  | Lst [ Sym "pair"; e1; e2 ] ->
      compile_exp defns tab stack_index e1 false
      @ [ Mov (stack_address stack_index, Reg Rax) ]
      @ compile_exp defns tab (stack_index - 8) e2 false
      @ [
          Mov (Reg R8, stack_address stack_index);
          Mov (MemOffset (Reg Rdi, Imm 0), Reg R8);
          Mov (MemOffset (Reg Rdi, Imm 8), Reg Rax);
          Mov (Reg Rax, Reg Rdi);
          Or (Reg Rax, Imm pair_tag);
          Add (Reg Rdi, Imm 16);
        ]
  | Lst [ Sym "left"; e ] ->
      compile_exp defns tab stack_index e false
      @ ensure_pair (Reg Rax)
      @ [ Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag))) ]
  | Lst [ Sym "right"; e ] ->
      compile_exp defns tab stack_index e false
      @ ensure_pair (Reg Rax)
      @ [ Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag + 8))) ]
  | Lst [ Sym "read-num" ] ->
      [
        Mov (stack_address stack_index, Reg Rdi);
        Add (Reg Rsp, Imm (align_stack_index stack_index));
        Call "read_num";
        Sub (Reg Rsp, Imm (align_stack_index stack_index));
        Mov (Reg Rdi, stack_address stack_index);
      ]
  | Lst [ Sym "print"; e ] ->
      compile_exp defns tab stack_index e false
      @ [
          Mov (stack_address stack_index, Reg Rdi);
          Mov (Reg Rdi, Reg Rax);
          Add (Reg Rsp, Imm (align_stack_index stack_index));
          Call "print_value";
          Sub (Reg Rsp, Imm (align_stack_index stack_index));
          Mov (Reg Rdi, stack_address stack_index);
          Mov (Reg Rax, operand_of_bool true);
        ]
  | Lst [ Sym "newline" ] ->
      [
        Mov (stack_address stack_index, Reg Rdi);
        Add (Reg Rsp, Imm (align_stack_index stack_index));
        Call "print_newline";
        Sub (Reg Rsp, Imm (align_stack_index stack_index));
        Mov (Reg Rdi, stack_address stack_index);
        Mov (Reg Rax, operand_of_bool true);
      ]
  | Lst (Sym "do" :: exps) when List.length exps > 0 ->
      List.mapi
        (fun i exp ->
          compile_exp defns tab stack_index exp
            (if i = List.length exps - 1 then is_tail else false))
        exps
      |> List.concat
  | _ -> raise (BadExpression exp)

let compile_defn (defns : defn list) { name; args; body } : directive list =
  let ftab =
    args |> List.mapi (fun i arg -> (arg, -8 * (i + 1))) |> Symtab.of_list
  in
  [ Label (defn_label name) ]
  @ compile_exp defns ftab (-8 * (List.length args + 1)) body true
  @ [ Ret ]

let compile (program : s_exp list) : string =
  let defns, body = defns_and_body program in
  [
    Global "entry";
    Extern "error";
    Extern "read_num";
    Extern "print_value";
    Extern "print_newline";
  ]
  @ [ Label "entry" ]
  @ compile_exp defns Symtab.empty (-8) body true
  @ [ Ret ]
  @ List.concat_map (compile_defn defns) defns
  |> List.map string_of_directive
  |> String.concat "\n"

let compile_to_file (program : string) : unit =
  let file = open_out "program.s" in
  output_string file (compile (parse_many program));
  close_out file

let compile_and_run (program : string) : unit =
  compile_to_file program;
  ignore (Unix.system "nasm program.s -f elf64 -o program.o");
  ignore (Unix.system "gcc program.o runtime.o -o program");
  ignore (Unix.system "./program")

let compile_and_run_io (program : string) (input : string) : string =
  compile_to_file program;
  ignore (Unix.system "nasm program.s -f elf64 -o program.o");
  ignore (Unix.system "gcc program.o runtime.o -o program");
  let inp, outp = Unix.open_process "./program" in
  output_string outp input;
  close_out outp;
  let r = input_all inp in
  close_in inp;
  r

let compile_and_run_err (program : string) (input : string) : string =
  try compile_and_run_io program input with BadExpression _ -> "ERROR"

let difftest (examples : (string * string) list) =
  let results =
    List.map
      (fun (ex, i) -> (compile_and_run_err ex i, Interp.interp_err ex i))
      examples
  in
  List.for_all (fun (r1, r2) -> r1 = r2) results

let test () = difftest [ ("(print (read-num))", "1") ]

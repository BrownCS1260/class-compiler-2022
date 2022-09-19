open S_exp
open Asm

exception BadExpression of s_exp

let num_shift = 2 
let num_mask = 0b11 
let num_tag = 0b00

let bool_shift = 7 
let bool_mask = 0b1111111
let bool_tag = 0b0011111

let rec compile_exp (exp : s_exp) : directive list = 
  match exp with
  | Num n ->
    [Mov (Reg Rax, Imm (n lsl num_shift))]
  | Sym "true" -> [Mov (Reg Rax, Imm ((1 lsl bool_shift) lor bool_tag))] 
  | Sym "false" -> [Mov (Reg Rax, Imm ((0 lsl bool_shift) lor bool_tag))]
  | Lst [Sym "add1"; arg] -> 
    compile_exp arg @
    [Add (Reg Rax, Imm (1 lsl num_shift))]
  | Lst [Sym "sub1"; arg] -> 
    compile_exp arg @
    [Sub (Reg Rax, Imm (1 lsl num_shift))]
  | _ -> raise (BadExpression exp) 

let compile (program : s_exp) : string =
  [Global "entry"; Label "entry"] @
  compile_exp program @
  [Ret]
  |> List.map string_of_directive |> String.concat "\n"

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

def hello := "world"

inductive s_exp 
| Num (i : Int)
| Sym (s : String)
| Lst (l : List s_exp)

#check s_exp.Lst []

open s_exp

def compile_exp : s_exp → Option (List directive)
-- | (Num n) => 
--   [Mov (Reg Rax, Imm (int.shiftl n num_shift))]
-- | (Sym "true") => [Mov (Reg Rax, Imm (int.lor (int.shiftl 1 bool_shift) bool_tag))]
-- | (Sym "false") := [Mov (Reg Rax, Imm (int.lor (int.shiftl 1 bool_shift) bool_tag))]
| (Lst [Sym "add1", arg]) => 
  compile_exp arg
-- | (Lst [Sym "sub1", arg]) := _
| _ => none
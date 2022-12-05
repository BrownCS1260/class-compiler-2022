inductive Register | Rax | Rcx 

def String_of_Register : Register → String 
| Register.Rax => "rax"
| Register.Rcx => "rcx"

inductive Operand 
| Reg (r : Register)
| Imm (i : Nat)

def is_Register : Operand → Bool 
| (Operand.Reg _) => true
| (Operand.Imm _) => false

def String_of_Operand : Operand → String 
| (Operand.Reg r) => String_of_Register r
| (Operand.Imm i) => toString i

inductive Directive 
| Global (s : String)
| Label (s : String)
| Mov (st : Operand × Operand)
| Add (st : Operand × Operand)
| Sub (st : Operand × Operand)
| Ret 
| Comment (s : String)

def String_of_Directive : Directive → String 
| (Directive.Global s) => "global " ++ s
| (Directive.Label s) => "label " ++ s ++ ":"
| (Directive.Mov st) => "\tmov " ++ String_of_Operand st.1 ++ ", " ++ String_of_Operand st.2
| (Directive.Add st) => "\tadd " ++ String_of_Operand st.1 ++ ", " ++ String_of_Operand st.2
| (Directive.Sub st) => "\tsub " ++ String_of_Operand st.1 ++ ", " ++ String_of_Operand st.2
| Directive.Ret => "\tret"
| (Directive.Comment s) => "; " ++ s




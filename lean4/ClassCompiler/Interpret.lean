import ClassCompiler.Asm 
import ClassCompiler.SExp 

open Directive Operand Register S_exp 

inductive Value 
| Integer (i : Int)
| Boolean (b : Bool)

def Value.to_String : Value → String 
| Integer i => toString i 
| Boolean b => toString b

instance : Repr Value where 
  reprPrec := λ v _ => Value.to_String v

open Value 

def interpret_exp : S_exp → Option Value
| (Num n) => Integer n
| (Sym "true") => Boolean true 
| (Sym "false") => Boolean false
| (Lst [Sym "add1", arg]) =>
  match interpret_exp arg with 
  | some (Integer i) => Integer (i + 1)
  | _ => none
| (Lst [Sym "sub1", arg]) => 
  match interpret_exp arg with 
  | some (Integer i) => Integer (i - 1)
  | _ => none
| _ => none

unsafe def interpret_string (s : String) : Option Value :=
let sexp := S_exp_of_String s; 
interpret_exp sexp

#eval interpret_string "(add1 4)"


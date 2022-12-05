import ClassCompiler.Process
import ClassCompiler.Compile 
import ClassCompiler.Interpret 

open Value S_exp

def Processor.evalToValue (ds : List Directive) : Value :=
match Processor.eval ds with 
| Sum.inl n => Integer n 
| Sum.inr b => Boolean b

#eval Processor.evalToValue (compile_string "(add1 (add1 10))")
#eval Processor.evalToValue (compile_string "true")

theorem land_helper (n : Nat) : (n <<< 2) &&& 3 = 0 :=
sorry

theorem shl_shr (n sh : Nat) : n <<< sh >>> sh = n :=
sorry

theorem correctness : ∀ prog : S_exp,
  -- (interpret_exp prog).isSome → 
    Processor.evalToValue (compile_exp prog) = interpret_exp prog  
| Num n => by 
  simp [Processor.evalToValue, Processor.eval, Processor.evalToState,
   processDirective, compile_exp, interpret_exp, List.foldl, ProcessorState.setReg,
   ProcessorState.opVal]
  simp [num_tag, num_shift, num_mask, land_helper, shl_shr] 
| Sym "true" => _
| Sym _ => _
| Lst [(Sym "add1"), x] => _
| Lst l => _ 

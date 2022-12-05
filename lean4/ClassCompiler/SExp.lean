import Lean.Data.Parsec

inductive S_exp 
| Num (i : Nat)
| Sym (s : String)
| Lst (l : List S_exp)  

partial def S_exp.to_String : S_exp → String 
| (S_exp.Num i) => toString i 
| (S_exp.Sym s) => s 
| (S_exp.Lst l) => "(" ++ String.join ((l.map S_exp.to_String).intersperse " ") ++ ")"

instance : Repr S_exp where 
  reprPrec := λ s _ => S_exp.to_String s

open Lean

def parse_num : Parsec S_exp :=
S_exp.Num <$> do 
  let dgs ← Parsec.many1 (Parsec.digit)
  pure dgs.toList.asString.toNat!


def parse_sym : Parsec S_exp :=
S_exp.Sym <$> (Parsec.manyChars $
  Parsec.satisfy $ λ ch => ch.isAlphanum ∨ ch ∈ ['+', '-', '*', '?'])
  
unsafe def parse_lst_aux (cont : Parsec S_exp) : Parsec (List S_exp) := do 
  let l ← cont
  (do 
    let _ ← Parsec.pchar ')' 
    pure [l]) <|>
  (do 
    let _ ← Parsec.pchar ' '
    List.cons l <$> parse_lst_aux cont)

unsafe def parse_lst (cont : Parsec S_exp) : Parsec S_exp :=
S_exp.Lst <$> (do 
  let _ ← Parsec.pchar '(' 
  parse_lst_aux cont)

unsafe def parse_S_exp : Parsec S_exp :=
parse_lst parse_S_exp <|> parse_num <|> parse_sym

unsafe def S_exp_of_String (s : String) : S_exp :=
match parse_S_exp.run s with 
| Except.ok s => s 
| Except.error _ => S_exp.Sym "ERROR"

#eval S_exp_of_String "(+ 4 4)"


theorem S_exp.induction {P : S_exp → Prop} (s : S_exp) 
  (nat : ∀ n, P (S_exp.Num n)) 
  (sym : ∀ s, P (S_exp.Sym s))  
  (lst : ∀ l, (∀ s, s ∈ l → P s) → P (S_exp.Lst l)) :
  P s :=
sorry
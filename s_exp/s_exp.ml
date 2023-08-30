type s_exp = Exp.t = Num of int | Sym of string | Lst of s_exp list

let show = Exp.show
let parse = Parser.parse
let parse_file = Parser.parse_file

let rec string_of_s_exp : s_exp -> string = function
  | Sym x ->
      x
  | Num n ->
      string_of_int n
  | Lst exps ->
      let exps = exps |> List.map string_of_s_exp in
      "(" ^ String.concat " " exps ^ ")"

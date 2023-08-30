type s_exp = Exp.t = Num of int | Sym of string | Lst of s_exp list

let show = Exp.show
let parse = Parser.parse
let parse_file = Parser.parse_file

(*//TODO: string_of_s_exp?*)
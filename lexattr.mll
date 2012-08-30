let blank = [' ' '\t' '\n' '\r']

rule add_style_acc acc style flag = parse
  | ("style"|"STYLE") blank* "=" blank*
      (('\'' ([^'\'']* as v) '\''
           | '"' ([^'"']* as v) '"'
           | ('#'?['a' - 'z' 'A' - 'Z' '0' - '9' '-' '+' '_' ':' '.']+ as v)))
(* '"' *)
 {
    add_style_acc
      (Printf.sprintf "%s style=\"%s;%s\"" acc v style) style true lexbuf
 }
  | _ as v { add_style_acc (acc ^ (String.make 1 v)) style flag lexbuf }
  | eof { if flag then acc else Printf.sprintf "%s style=\"%s\"" acc style}

{
  let add_style style attrs =
    add_style_acc "" style false (MyLexing.from_string attrs)
}

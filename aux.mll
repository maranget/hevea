{
open Lexing

let rtable = Hashtbl.create 17
;;

let rset name value = Hashtbl.add rtable name value
;;

let rget name =
  try Hashtbl.find rtable name with Not_found -> "X"
;;

let btable = Hashtbl.create 17
;;

let bset name value = Hashtbl.add btable name value
;;

let bget name =
  try Hashtbl.find btable name with Not_found -> name
;;

}

rule main = parse
  "\\newlabel"
    {let name = Save.arg lexbuf in
    let value = Save.arg lexbuf in
    let value = Save.arg (from_string value) in
    rset name value ; main lexbuf}
| "\\bibcite"
    {let name = Save.arg lexbuf in
    let value = Save.arg lexbuf in
    bset name value ;
    main lexbuf}
| _   {main lexbuf}
| eof {()}

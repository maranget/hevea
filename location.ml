let stack = ref []
;;
let push s e = s := e:: !s
and pop s = match !s with
  [] -> failwith "Location : Empty stack"
| e::rs -> s := rs ; e
;;



let curlexbuf = ref (Lexing.from_string "")
and curlexname = ref ""
;;

let set name lexbuf =
  push stack (!curlexname,!curlexbuf) ;
  curlexname := name ;
  curlexbuf := lexbuf
;;

let restore () =
  let name,lexbuf = pop stack in
  curlexname := name ;
  curlexbuf := lexbuf
;;


let print_pos () =
  prerr_string ("Error in file: "^ !curlexname^" ") ;
  Printf.fprintf stderr "%d -> %d"
    (Lexing.lexeme_start !curlexbuf)
    (Lexing.lexeme_end !curlexbuf) ;
  prerr_endline ""
;;


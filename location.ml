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

let get () = !curlexname
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


let rec find_line file r = function
  0 -> r
| n ->
   find_line file
    (match input_char file with '\n' -> r+1 | _ -> r)
    (n-1)
;;

   
let print_pos () =
  try
    let file = open_in !curlexname in
    let nline = find_line file 1 (Lexing.lexeme_start !curlexbuf) in
    close_in file ;
    prerr_string (!curlexname^":"^string_of_int nline^": ")
  with Sys_error s ->
     prerr_endline
       ("Trouble in print_pos, file: "^ !curlexname^
       ", "^s)
         
;;


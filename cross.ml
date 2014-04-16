(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

let verbose = ref 0
;;

let table = Hashtbl.create 37
;;

let add name file =  
  if !verbose > 0 then
      prerr_endline ("Register "^name^" in "^file) ;
  try
    let _ = Hashtbl.find table name in
    Location.print_pos () ;
    prerr_endline ("Warning, multiple definitions for anchor: "^name) ;
  with
  | Not_found ->
      Hashtbl.add table name file
;;


let decode_fragment frag =
  let buff = Buffer.create 32 in
  Url.decode_fragment (Buffer.add_char buff) (Buffer.add_string buff) frag ;
  Buffer.contents buff

let fullname change myfilename name =
  if !verbose > 1 then
    Printf.eprintf "FULL: filename=%s, name=%s ->" myfilename name ;
  let r = 
    try
      let filename = Hashtbl.find table (decode_fragment name) in
      let newname =
        if myfilename = filename  then
	  "#"^name
        else
          change filename^"#"^name in
    if !verbose > 1 then
      prerr_endline ("From "^name^" to "^newname) ;
      newname
    with Not_found -> begin
      Location.print_pos () ;
      prerr_endline ("Warning, cannot find anchor: "^name) ;
      raise Not_found
    end in
  if !verbose > 1 then Printf.eprintf " %s\n" r ;
  r
;;

let dump outname change =
  try
    let chan = open_out outname in
    try
      Hashtbl.iter
        (fun k x -> Printf.fprintf chan "%s\t%s\n" k (change x))
        table ;
      close_out chan
    with
    | e -> close_out chan ; raise e
  with
  | Sys_error msg ->
      prerr_endline ("Error while dumping "^outname^": "^msg)
      
        



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

let header = "$Id: cross.ml,v 1.6 1998-07-21 11:18:26 maranget Exp $" 
let verbose = ref 0
;;

let table = Hashtbl.create 37
;;

let add name file =  
  Hashtbl.add table name file ;
  if !verbose > 0 then
      prerr_endline ("Register "^name^" in "^file)
;;


let fullname name =
  try
    let filename = Hashtbl.find table name in
    let newname = filename^"#"^name in
    if !verbose > 0 then
      prerr_endline ("From "^name^" to "^newname) ;
    newname
  with Not_found -> begin
    Location.print_pos () ;
    prerr_endline ("Warning, Cross.find, cannot find label: "^name) ;
    raise Not_found
  end
;;

    

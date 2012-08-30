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

let uptable = Hashtbl.create 17
and nexttable = Hashtbl.create 17
and prevtable = Hashtbl.create 17
;;

let setup file upname = Hashtbl.add uptable file upname
and setprev file prevname = Hashtbl.add prevtable file prevname
let setnext file nextname = Hashtbl.add nexttable file nextname
;;

let setprevnext prev now =
  if prev <> "" then begin
    Hashtbl.add nexttable prev now ;
    Hashtbl.add prevtable now prev
  end
;;

let next name = Hashtbl.find nexttable name
and up   name = Hashtbl.find uptable name
and prev name = Hashtbl.find prevtable name
;;

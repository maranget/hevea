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

let header = "$Id: thread.ml,v 1.2 1998-07-21 11:18:44 maranget Exp $" 
let uptable = Hashtbl.create 17
and nexttable = Hashtbl.create 17
and prevtable = Hashtbl.create 17
;;

let setup file upname = Hashtbl.add uptable file upname
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

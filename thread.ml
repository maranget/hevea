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

let header = "$Id: thread.ml,v 1.3 2000-03-28 13:52:57 maranget Exp $" 
let uptable = Hashtbl.create 17
and nexttable = Hashtbl.create 17
and prevtable = Hashtbl.create 17
;;

let setup file upname = Hashtbl.add uptable file (ref upname)
;;

let setprevnext prev now =
  if prev <> "" then begin
    Hashtbl.add nexttable prev (ref now) ;
    Hashtbl.add prevtable now (ref prev)
  end
;;

let next name = !(Hashtbl.find nexttable name)
and up   name = !(Hashtbl.find uptable name)
and prev name = !(Hashtbl.find prevtable name)
;;

let change_aux t oldname name =
  let olds = Hashtbl.find_all t oldname in
  List.iter
    (fun s ->
      Hashtbl.remove t oldname ;
      Hashtbl.add t name s)
    olds ;
  Hashtbl.iter
    (fun k x ->
      if !x = oldname then begin
        x := name
      end)
    t
  
let change oldname name =
  change_aux nexttable oldname name ;
  change_aux prevtable oldname name ;
  change_aux uptable oldname name


(* <Christian.Queinnec@lip6.fr>
 The plugin for HeVeA that implements the VideoC style.
 $Id: videoc.mli,v 1.2 1999-05-10 15:54:06 tessaud Exp $
*)

module type T =
  sig
    val init : unit -> unit
  end;;

module Makealso (Dest : OutManager.S) (Scan : Latexscan.S) : T

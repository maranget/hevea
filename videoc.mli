
(* <Christian.Queinnec@lip6.fr>
 The plugin for HeVeA that implements the VideoC style.
 $Id: videoc.mli,v 1.1 1999-03-08 18:37:40 maranget Exp $
*)

module type T =
  sig
    val init : unit -> unit
  end;;

module Makealso (Scan : Latexscan.S) : T


(* <Christian.Queinnec@lip6.fr>
 The plugin for HeVeA that implements the VideoC style.
 $Id: videoc.mli,v 1.3 1999-06-03 13:13:36 maranget Exp $
*)

module type T =
  sig
    val init : unit -> unit
  end;;

module Makealso
    (Dest : OutManager.S) (Image : ImageManager.S) (Scan : Latexscan.S) : T

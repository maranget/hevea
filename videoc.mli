
(* <Christian.Queinnec@lip6.fr>
 The plugin for HeVeA that implements the VideoC style.
 $Id: videoc.mli,v 1.4 1999-09-24 16:25:44 maranget Exp $
*)

module type T =
  sig
    val init : unit -> unit
  end;;

module Makealso
    (Dest : OutManager.S) (Image : ImageManager.S)
    (Scan : Latexscan.S) (Lexget : Lexget.S) : T

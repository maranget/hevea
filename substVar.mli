(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Cambium, INRIA Paris                          *)
(*                                                                     *)
(*  Copyright 2020 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

module Make :
  functor
    (C:sig
      val put : string -> unit
      val put_char : char -> unit
      val to_string : unit -> string
    end) ->
      sig
        val subst : string (* body *) -> string (* arg *) -> unit
        val to_string : unit -> string
      end


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

module type S =
  sig

    val start : unit -> unit
    val stop : unit -> unit
    val restart : unit -> unit

    val put_char : char -> unit
    val put : string -> unit

    val dump :  string -> (Lexing.lexbuf -> unit) -> Lexing.lexbuf -> unit
    val page : unit -> unit

    val finalize : bool -> bool
  end

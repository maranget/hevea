(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: stack.mli,v 1.6 2001-05-25 09:20:49 maranget Exp $"            *)
(***********************************************************************)
exception Fatal of string

type 'a t
val create : string ->  'a t
val create_init : string -> 'a ->  'a t
val name : 'a t -> string
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a
val top : 'a t -> 'a
val pretty : ('a -> string) -> 'a t -> unit
val length : 'a t -> int
val empty : 'a t -> bool
val rev : 'a t -> unit
val map : 'a t -> ('a -> 'a) -> unit

type 'a saved
val empty_saved : 'a saved
val save : 'a t -> 'a saved
val restore : 'a t -> 'a saved -> unit
val finalize : 'a t -> ('a -> bool) -> ('a -> unit) -> unit
(*
  finalize now p f
    apply f to now elements until
    now is empty or p  is true for one element
*)

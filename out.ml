(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Tibault Suzanne, Luc Maranget, projet Moscova, INRIA Rocquencourt  *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

include DoOut.Make
    (struct
      let small_length =
        let x =  !Parse_opts.small_length in
        if x <= 0 then 1 else x
    end)

(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: hot.ml,v 1.6 2006-07-26 18:16:05 maranget Exp $               *)
(***********************************************************************)
type saved =
    Misc.saved * Lexstate.saved * Latexmacros.saved *
      Counter.saved * Color.saved * Foot.saved

let checkpoint () =
  Misc.checkpoint (),
  Lexstate.checkpoint (),
  Latexmacros.checkpoint (),
  Counter.checkpoint (),
  Color.checkpoint (),
  Foot.checkpoint ()

and start (misc, lexstate, latexmacros, counter, color, foot) =
  Misc.hot_start misc ;
  Lexstate.hot_start lexstate ;
  Latexmacros.hot_start latexmacros ;
  Counter.hot_start counter ;
  Color.hot_start color ;
  Foot.hot_start foot ;
  begin match !Parse_opts.destination with
  | Parse_opts.Info -> InfoRef.hot_start ()
  | _ -> ()
  end


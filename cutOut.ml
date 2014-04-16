(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet MOSCOVA, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2006 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


module type Config = sig
  val small_length : int
end

module Make(C:Config) = struct

module Out = DoOut.Make(C)

type t = { out : Out.t ; name : string }

let get_name { name = name } = name

let create name do_it = { out = do_it () ; name = name }

let create_buff name = create name Out.create_buff
and create_null () = create "NULL" Out.create_null
and create_chan name = 
  create name (fun () -> Out.create_chan (open_out name))
and close { out = out } = Out.close out

let put { out = out } s = Out.put out s
and put_char { out = out } c = Out.put_char out c
and is_empty { out = out } = Out.is_empty out
and to_string { out = out } = Out.to_string out
and to_chan chan { out = out } = Out.to_chan chan out
and copy { out = out1 } { out = out2 } = Out.copy out1 out2
and flush { out = out } = Out.flush out
let debug chan { out; name; } =
  Printf.fprintf chan "Out=%s\n" name ;
  Out.debug chan out ;
  ()
end

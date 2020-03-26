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

{
module Make
   (C:
      sig
        val put : string -> unit
        val put_char : char -> unit
        val to_string : unit -> string
      end
    ) = struct
} 

rule main  arg  = parse
| "VAR" { C.put arg ; main arg lexbuf }
| _ as a { C.put_char a ; main arg lexbuf } 
| eof { () }

{
  let subst macro arg = main arg (Lexing.from_string macro)

  let to_string = C.to_string
end
} 

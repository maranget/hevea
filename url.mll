(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

(* URL encoding and decoding, As the issue is still pending, apply to fragment only! *)

{
 open Printf

 type url =
 {
  scheme : string option ;
  authority : string option ;
  path : string ;
  query : string option ;
  fragment : string option ;
 }

 exception Error
}

let hex = ['0'-'9''A'-'F''a'-'f']

rule break = parse
|
([^':''/''?''#']+ as scheme ':') ?
("//" ([^'/''?''#']* as authority)) ?
([^'?''#']* as path)
('?' [^'#']* as query)?
('#' (_* as fragment))?
{ {scheme; authority; path; query; fragment;} }
| "" { raise Error }

and do_decode putc = parse
| '%' (hex as a) (hex as b)
  { let n =
    try int_of_string (sprintf "0x%c%c" a b) with _ -> assert false in
  putc (Char.chr n) ;
  do_decode putc lexbuf }
| _ as c { putc c ; do_decode putc lexbuf }
| eof    { () }

{
(* See
http://www.lunatech-research.com/archives/2009/02/03/what-every-web-developer-must-know-about-url-encoding/#Thereservedcharactersarenotwhatyouthinktheyare
*)
 let do_encode_fragment putc put c =  match c with
 |  'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '~' | '.'
   -> putc c
 | _ -> put (sprintf "%%%02X" (Char.code c))

 let do_encode putc put specific u =
   let len = String.length u in
   for k =0 to len-1 do
     let c = String.unsafe_get u k in
     specific putc put c
   done

 let apply putc put f u =
   begin match u.scheme with
   | None -> ()
   | Some s -> f s ; putc ':'
   end ;
   begin match u.authority with
   | None -> ()
   | Some s -> put "//" ; f s
   end ;
   f u.path ;
   begin match u.query with
   | None -> ()
   | Some s -> putc '?' ; f s
   end ;
   begin match u.fragment with
   | None -> ()
   | Some s -> putc '#' ; f s
   end ;
   ()

 let _encode putc put u =
   let u = break (MyLexing.from_string u) in
   apply putc put (do_encode putc put do_encode_fragment) u

 let _decode putc put u =
   let u = break (MyLexing.from_string u) in
   let do_decode s = do_decode putc (MyLexing.from_string s) in
   apply putc put do_decode u


  let encode_fragment putc put u =
    do_encode putc put do_encode_fragment u

  let decode_fragment putc _put u = do_decode putc (MyLexing.from_string u)
}

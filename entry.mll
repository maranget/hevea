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

{

let buff = Out.create_buff ()
;;

let put s =
  Out.put buff s
and put_char c =
  Out.put_char buff c
;;


type res =
| Bang of string * string
| Bar of string * string
| Eof of string * string
;;

let extend r i = match r with
| Bang (p,_) -> Bang (i,p)
| Bar (p,_) -> Bar (i,p)
| Eof (p,_) -> Eof (i,p)
;;

type key = string list * string list

exception Fini
exception NoGood
;;

}
rule entry = parse
| "\\\""
    {put "\\\"" ; entry lexbuf}
| '"' (_ as lxm)
    {put_char lxm ; entry lexbuf}
| '!' {Bang   (Out.to_string buff,"")}
| '@'
    {let s = Out.to_string buff in
    let r = entry lexbuf in
    extend r s}
| '|' {Bar (Out.to_string buff,"")}
| eof {Eof (Out.to_string buff,"")}
| _ as lxm {put_char lxm ; entry lexbuf}      

and idx = parse
|  "\\indexentry"
     {let key = Save.arg lexbuf in
     let  value = Save.arg lexbuf in
     key,value}
| eof {raise Fini}
| _   {idx lexbuf}


{

let read_key lexbuf =
    
  let bar () = match entry lexbuf with
  | Eof (s,_) ->
      begin match s with
      | ""|"("|")" -> None
      | s ->
          if s.[0] = '(' then
            Some (String.sub s 1 (String.length s - 1))
          else
            Some s
      end
  | _         -> raise NoGood in

  let rec get_rec () = match entry  lexbuf with
    Bang (i,p) ->
      let l,see = get_rec () in
      (i,p)::l,see
  | Bar (i,p) ->
      let see = bar () in
      [i,p],see
  | Eof (i,p) -> [i,p],None in

  let separe (l,see) =
    let rec sep_rec = function
      [] -> [],[]
    | (x,y)::r ->
        if x="" then raise NoGood ;          
        let xs,ys = sep_rec r in
        x::xs,y::ys in
    let xs,ys = sep_rec l in
    ((xs,ys),see) in

  separe (get_rec ())

let read_indexentry lexbuf = idx lexbuf
} 

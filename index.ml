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

let header = "$Id: index.ml,v 1.12 1998-07-21 11:18:32 maranget Exp $" 
open Parse_opts
open Entry

let comp (l1,p1) (l2,p2) =
  let rec c_rec l1 l2 = match l1,l2 with
  [],[] -> compare p1 p2
| [],_  -> if p1 <> "" then 1 else -1
| _,[]  -> 1
| x1::r1,x2::r2 ->
     let t = compare x1 x2 in
     if t=0 then c_rec r1 r2 else t in
  c_rec l1 l2
;;

type key = string list * string
;;

type entry = key * string
;;

let pretty_key (l,s1) =
 let rec p_rec = function
   [] -> ""
 | [x] -> x
 | x::xs -> x^"!"^p_rec xs in
 p_rec l^"@"^s1
;;

let pretty_entry (k,_) = pretty_key k
;;

exception NoGood of string
;;

let read_index lexbuf =
    
  let bar () = match entry lexbuf with
    Eof s -> s
  | _     ->  raise (NoGood "") in

  let arobas () = match entry lexbuf with
    Eof s -> s,""
  | Bar s -> s,bar ()
  | _     ->  raise (NoGood "") in

  let rec get_rec () = match entry  lexbuf with
    Bang s ->
      let (l,s1),s2 = get_rec () in
      (s::l,s1),s2
  | Arobas s ->
      let s1,s2 = arobas () in
      ([s],s1),s2
  | Bar s ->
      let s2 = bar () in
      ([s],""),s2
  | Eof s ->
      ([s],""),"" in
  get_rec ()
;;

module OrderedKey = struct
  type t = key
  let compare = comp
end
;;

module KeySet =  Set.Make(OrderedKey)
;;

open KeySet


type idx = No | Yes of entry array
;;

let (itable:
   (string,
   string * KeySet.t ref * (key, (string * int * string)) Hashtbl.t * int ref * idx)
   Hashtbl.t) = Hashtbl.create 17
;;

let bad_entry = (([],""),"")
;;

let newindex tag suf name =
  let basename = Location.get_base () in
  let filename = basename^"."^suf in
  let idxstruct =
    if !read_idx then begin
      try      
        let fullname,chan = Myfiles.open_tex filename in
        let lexbuf = Lexing.from_channel chan in
        let rec do_rec () = try
          let arg = Entry.idx lexbuf in
          let k =
            try read_index (Lexing.from_string arg) 
            with NoGood _ -> raise (NoGood arg) in
          k::do_rec ()
        with Entry.Fini -> []
        | NoGood arg -> begin
           if not !silent then begin
             Location.print_pos () ;
             prerr_endline
             ("Warning, bad index arg syntax in file: "^filename^
             " arg is "^arg)
           end ;
           bad_entry::do_rec ()
        end in

        let r = do_rec () in
        if !verbose > 0 then
          prerr_endline ("Index file: "^fullname^" succesfully read");
        Yes (Array.of_list r)     
      with  Myfiles.Error msg -> begin
        prerr_endline ("Index: "^msg^", I try to manage") ;
        No end
      | Myfiles.Except -> begin
        if !verbose > 0 then
          prerr_endline ("Not reading file: "^filename);
        No end      
      end else No in
  let all = ref empty
  and table = Hashtbl.create 17
  and count = ref 0 in
  Hashtbl.add itable tag (name,all,table,count,idxstruct)
;;


let treat tag arg =
  try
    if !verbose > 2 then prerr_endline ("Index.treat with arg: "^arg) ;
    let name,all,table,count,idxstruct = Hashtbl.find itable tag in
    let lexbuf = Lexing.from_string arg in
    let key = 
      let ((ka,_),_ as key_arg) =
         try read_index lexbuf with
         NoGood _ -> begin
           if not !silent then begin
             Location.print_pos () ;
             prerr_endline
               ("Warning, bad index arg syntax: "^arg)
           end ;
           bad_entry
         end
      and ((ki,_),_ as key_idx) = match idxstruct with
        No    -> bad_entry
      | Yes t ->
       if !count >= Array.length t then bad_entry
       else t.(!count) in
       if ka = ki then key_idx
       else if key_arg = bad_entry then key_idx
       else key_arg in
    if !verbose > 2 then
      prerr_endline ("Finally arg is: "^pretty_entry key) ;
    let label = ("@"^tag^string_of_int !count) in
    if key <> bad_entry then begin
      Html.loc_name label "" ;
      let key,macro = key in
      Hashtbl.add table key (label,!count,macro) ;
      all := add key !all
    end ;
    count := !count + 1
  with Not_found -> begin
    Location.print_pos () ;
    prerr_endline ("Index: "^tag^" is undefined, makeindex or newindex is missing")
  end
;;

      
let rec common e1 e2 p1 p2 = match e1,e2 with
  [],_  -> [],e2
| _,[]  -> e1,[]
| [x1],[x2] -> e1,e2
| x1::r1,[x2] -> e1,e2
| x1::r1,x2::r2 ->
    if x1=x2 then
      common r1 r2 p1 p2
    else
      e1,e2
;;
let rec close_prev = function
  []| [_] -> ()
| _::r    ->  Html.close_block "UL" ; close_prev r
;;

let rec open_this  main k pk = match k with
  [] -> ()
| k::r ->
    Html.item
      (fun tag ->
      try
        main tag
      with x -> begin
          prerr_endline ("Something wrong with index: "^tag) ;
          raise x
      end) (if r = [] && pk <> "" then pk else k) ;
    begin match r with
      [] -> ()
    | _  -> Html.open_block "UL" "" 
    end ;
    open_this main r pk
;;


let print_entry main (bk,bpk) (k,pk) xs  =
  let rp,rt = common bk k pk bpk in
  close_prev rp ;
  if rp = [] then Html.open_block "UL" "" ;
  open_this main rt pk ;  
  let rec prints = function
    [] -> ()
  | (label,x,m)::r ->
      let arg = match m with
        "" -> string_of_int x
      | _  -> "\\"^m^"{"^string_of_int x^"}" in
      let no_ref = String.length m > 3 && String.sub m 0 3 = "see" in
      main (if no_ref then arg
           else "\\indexref{"^arg^"}{"^label^"}") ;
      if r <> [] then begin
        Html.put ", " ;
        prints r
      end in
   Html.put "&nbsp;&nbsp;" ;
   prints (List.rev xs)
;;

     
    
let print main  tag =
  let name,all,table,_,_ = Hashtbl.find itable tag in
  main ("\\@indexsection{"^name^"}") ;
  let prev = ref ([],"") in
  KeySet.iter (fun k ->
    if !verbose > 2 then
      prerr_endline ("Print_entry: "^pretty_key k);
    print_entry main !prev k (Hashtbl.find_all table k) ;
    prev := k)
 !all ;
 let pk,_ = !prev in
 List.iter (fun _ -> Html.close_block "UL") pk ;
;;

  

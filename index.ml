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
module type T =
  sig
    exception Error of string
    val newindex : string -> string -> string -> unit
    val treat: (string -> bool) -> string -> string -> unit
    val print: (string -> unit) -> string -> unit
  end

module Make (Html : OutManager.S) =
struct

let header = "$Id: index.ml,v 1.19 1999-04-07 19:24:51 maranget Exp $"
open Misc
open Parse_opts
open Entry

exception Error of string

let comp (l1,p1) (l2,p2) =

  let rec c_rec k l1 l2 = match l1,l2 with
  [],[] -> k p1 p2
| [],_  -> -1
| _,[]  -> 1
| x1::r1,x2::r2 ->
     let t = compare (String.capitalize x1) (String.capitalize x2) in
     if t=0 then c_rec k r1 r2 else t in
  c_rec (fun p1 p2 -> c_rec (fun _ _ -> 0) p1 p2) l1 l2
;;

type key = string list * string list
;;

type entry = key * string
;;

let pretty_key (l,p) =
 let rec p_rec l p = match l,p with
   [],[] -> ""
 | [x],[""]-> x
 | [x],[y]-> x^"@"^y
 | x::xs,""::ys -> x^"!"^p_rec xs ys
 | x::xs,y::ys -> x^"@"^y^"!"^p_rec xs ys
 |  _,_ -> assert false in
 p_rec l p
;;

let pretty_entry (k,_) = pretty_key k
;;

exception NoGood of string
;;

let read_index lexbuf =
    
  let bar () = match entry lexbuf with
    Eof (s,_) -> s
  | _         ->  raise (NoGood "") in

  let rec get_rec () = match entry  lexbuf with
    Bang (i,p) ->
      let l,see = get_rec () in
      (i,p)::l,see
  | Bar (i,p) ->
      let see = bar () in
      [i,p],see
  | Eof (i,p) -> [i,p],"" in

  let separe (l,see) =
    let rec sep_rec = function
      [] -> [],[]
    | (x,y)::r ->
        let xs,ys = sep_rec r in
        x::xs,y::ys in
    let xs,ys = sep_rec l in
    ((xs,ys),see) in

  separe (get_rec ())
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

let bad_entry = (([],[]),"")
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
        with
        | Entry.Fini -> []
        | NoGood arg -> begin
             Parse_opts.warning
              ("Warning, bad index arg syntax in file: "^filename^
               " arg is "^arg) ;
            bad_entry::do_rec ()
        end in

        let r = do_rec () in
        if !verbose > 0 then
          prerr_endline ("Index file: "^fullname^" succesfully read");
        Yes (Array.of_list r)     
      with
      | Myfiles.Error msg -> begin
          Parse_opts.warning
            ("Index: "^msg^", I try to manage") ;
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


let check_key lexcheck ((l,p),_) =
  let rec check_rec xs ys = match xs,ys with
    | [],[] -> true
    | x::xs,""::ys ->
        lexcheck x &&
        check_rec xs ys
    | _::xs,y::ys ->
        lexcheck y &&
        check_rec xs ys
    | _,_ -> assert false in
  let r = check_rec l p in
  if not r then begin
    raise (Error ("Index key ``"^pretty_key (l,p)^"'' is incorrect")) ;
  end ;
  r

let find_index tag =
  try Hashtbl.find itable tag
  with Not_found ->
    raise (Error ("Missing \makeindex for index: "^tag))

let treat lexcheck tag arg =
  try
    if !verbose > 2 then prerr_endline ("Index.treat with arg: "^arg) ;
    let name,all,table,count,idxstruct = find_index tag in
    let lexbuf = Lexing.from_string arg in
    let key = 
      let ((ka,_),_ as key_arg) =
         try read_index lexbuf with
         NoGood _ -> begin
           bad_entry
         end
      and ((ki,_),_ as key_idx) = match idxstruct with
        No    -> bad_entry
      | Yes t ->
       if !count >= Array.length t then bad_entry else t.(!count) in
       if key_idx = bad_entry then key_arg
       else key_idx in
    if !verbose > 2 then
      prerr_endline ("Finally arg is: "^pretty_entry key) ;
    let label = ("@"^tag^string_of_int !count) in
    if key <> bad_entry && check_key lexcheck key then begin
      Html.loc_name label "" ;
      let key,macro = key in
      Hashtbl.add table key (label,!count,macro) ;
      all := add key !all
    end else 
      Parse_opts.warning ("Warning, bad index arg syntax: "^arg) ;
    count := !count + 1 ;
    if !verbose > 2 then
      prerr_endline ("Treat out: count="^string_of_int !count)
  with Not_found -> begin
    Parse_opts.warning
      ("Index: "^tag^" is undefined, makeindex or newindex is missing")
  end
;;

      
let rec common e1 e2 = match e1,e2 with
  ([],_),_        -> e1,e2
| _,([],_)        -> e1,e2
| ([_],_),([_],_) -> e1,e2
| (_::_,_),([_],_) -> e1,e2
| (x1::r1,_::p1),(x2::r2,_::p2) ->
    if x1=x2 then
      common (r1,p1) (r2,p2)
    else
      e1,e2
|  _ -> assert false
;;
let rec close_prev = function
  [],_ | [_],_ -> ()
| _::r,_::p    ->  Html.close_block "UL" ; close_prev (r,p)
|  _ -> assert false
;;

let rec open_this  main k = match k with
  [],_ -> ()
| k::r,p::rp ->
    Html.item
      (fun tag ->
      try
        main tag
      with x -> begin
          prerr_endline ("Something wrong with index: "^tag) ;
          raise x
      end) (if p <> "" then p else k) ;
    begin match r with
      [] -> ()
    | _  -> Html.open_block "UL" "" 
    end ;
    open_this main (r,rp)
|  _ -> assert false
;;


let print_entry main bk k xs  =
  let rp,rt = common bk k in
  close_prev rp ;
  if fst rp = [] then Html.open_block "UL" "" ;
  open_this main rt ;  
  let rec prints = function
    [] -> ()
  | (label,x,m)::r ->
      let arg = match m with
      |  "" -> string_of_int x
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
  if !verbose > 1 then prerr_endline ("Print index ``"^tag^"''") ;
  let name,all,table,_,_ = find_index tag in
  main ("\\@indexsection{"^name^"}") ;
  let prev = ref ([],[]) in
  KeySet.iter (fun k ->
    if !verbose > 2 then
      prerr_endline ("Print_entry: "^pretty_key k);
    print_entry main !prev k (Hashtbl.find_all table k) ;
    prev := k)
 !all ;
 let pk,_ = !prev in
 List.iter (fun _ -> Html.close_block "UL") pk ;
;;

  
end

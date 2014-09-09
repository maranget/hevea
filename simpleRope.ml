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

open Printf

exception Out_of_bounds


module type Config = sig
  val small_length : int
end


module Make(C:Config) = struct
  open C
(**********)
(* Basics *)
(**********)

  type t =
    | Str of string 
    | App of t * t * int  (* String length *)

  let length = function
    | Str s -> String.length s
    | App (_,_,len) -> len

  let of_string s = Str s

  let singleton c = of_string (String.make 1 c)

  let empty = of_string ""

(**********)
(* Append *)
(**********)

  let app r1 r2 = match r1,r2 with
  | Str "",t | t,Str "" -> t
  | Str s1, Str s2
    when String.length s1 < small_length && String.length s2 < small_length ->
      Str (s1^s2)
  | App (t1,Str s1,len), Str s2
    when String.length s1 < small_length && String.length s2 < small_length ->
      App (t1,Str (s1^s2),len+String.length s2)
  | Str s1,App (Str s2,t2,len)
    when String.length s1 < small_length && String.length s2 < small_length ->
      App (Str (s1^s2),t2,len+String.length s1)
  | _,_ ->
      App (r1,r2,length r1+length r2)


  let append r1 r2 = app r1 r2

  let rec app_string r s slen = match r with
  | Str rs ->
      if String.length rs < small_length then Str (rs ^ s)
      else raise Exit
  | App (r1,r2,len) ->
      let r2 = app_string r2 s slen in
      App (r1,r2,len+slen)

  let append_string r s =
    let slen = String.length s in
    if slen < small_length then
      try app_string r s slen
      with Exit -> App (r,Str s,length r+slen)
    else App (r,Str s,length r+slen)

  let sc2c s len c =
    let b = Bytes.create (len+1) in
    Bytes.blit_string s 0 b 0 len ;
    Bytes.set b len c ;
    Bytes.unsafe_to_string b

  let rec app_char r c = match r with
  | Str s ->
      let len = String.length s in
      if len < small_length then begin
        Str (sc2c s len c)
      end else
        raise Exit
  | App (r1,r2,len) ->
      let r2 = app_char r2 c in
      App (r1,r2,len+1)

  let append_char r c =
    try app_char r c
    with Exit -> App (r,Str (String.make 1 c),length r+1)

(*************)
(* Substring *)
(*************)

(* assumption: 0 <= start < stop <= len(t) *)
  let rec mksub start stop t =
    if start = 0 && stop = length t then t 
    else match t with
    | Str s -> Str (String.sub s start (stop-start))
    | App (t1, t2, _) ->
        let n1 = length t1 in
        if stop <= n1 then mksub start stop t1 
        else if start >= n1 then mksub (start-n1) (stop-n1) t2
        else app (mksub start n1 t1) (mksub 0 (stop-n1) t2)
            
  let sub t ofs len = 
    let stop = ofs + len in
    if ofs < 0 || len < 0 || stop > length t then raise Out_of_bounds;
    if len = 0 then empty else mksub ofs stop t
      
(***********************)
(* Get a char by index *)
(***********************)

  let rec get_rec t i = match t with
  | Str s -> String.unsafe_get s i
  | App (t1, t2, _) ->
      let n1 = length t1 in
      if i < n1 then get_rec t1 i else get_rec t2 (i - n1)


  let get t i = 
    if i < 0 || i >= length t then raise Out_of_bounds;
    get_rec t i

(***********)
(* Iterate *)
(***********)

  let iter_string f s =
    for k=0 to String.length s-1 do
      f (String.unsafe_get s k)
    done

  let rec iter_rec f = function
    | Str s -> iter_string f s
    | App (t1,t2,_) ->
        iter_rec f t1 ;
        iter_rec f t2


  let iter f t = iter_rec f t

(**********)
(* Output *)
(**********)

let rec output chan = function
  | Str s -> output_string chan s
  | App (t1,t2,_) -> output chan t1 ; output chan t2

let rec debug_rec indent chan = function
 | Str s ->
     fprintf chan "%s\"%a\"\n" indent output_string s
 | App (t1,t2,_) ->
     let indent2 = indent ^ "  " in
     fprintf chan "%s[\n" indent ;
     debug_rec indent2 chan t1 ;
     debug_rec indent2 chan t2 ;
     fprintf chan "%s]\n" indent ;
     ()

let debug = debug_rec ""

(*************)
(* To string *)
(*************)

let rec blit t buff pos = match t with
 | Str s ->
     Bytes.blit_string s 0 buff pos (String.length s)
 | App (t1,t2,_) ->
     blit t1 buff pos ;
     blit t2 buff (pos+length t1)

let to_string t = match t with
| Str s -> s
| App (_,_,len) ->
    let buff = Bytes.create len in
    blit t buff 0 ;
    Bytes.unsafe_to_string buff

(***********************)
(* To list (of string) *)
(***********************)

let rec do_to_list k = function
  | Str s -> if String.length s > 0 then (s::k) else k
  | App (t1,t2,_) ->
      let k = do_to_list k t2 in
      do_to_list k t1

let to_list t = do_to_list [] t
let to_list_append t k = do_to_list k t

(*******************)
(* Index functions *)
(*******************)

let rec index_from r i c = match r with
| Str s -> String.index_from s i c
| App (t1,t2,_) ->
    let n1 = length t1 in
    if i < n1 then
      try index_from t1 i c
      with Not_found -> index_from t2 0 c + n1
    else index_from t2 (i-n1) c

let index r c =
  try index_from r 0 c
  with e ->
    eprintf "SimpleRope.index failed c='%c'\n" c ;
    debug stderr r ;
    raise e

let rec rindex_from r i c = match r with
| Str s -> String.rindex_from s i c
| App (t1,t2,_) ->
    let n1 = length t1 in
    if i < n1 then rindex_from t1 i c
    else
      try rindex_from t2 (i-n1) c + n1
      with Not_found -> rindex_from t1 (n1-1) c

let rindex r c = rindex_from r (length r-1) c

(* Erase end according to predicate *)
  let erase t pred =
    let rec do_rec t = match t with
    | Str s ->
        let len = String.length s in
        let rec find_no k =
          if k <= 0 then k
          else
            let c = String.unsafe_get s (k-1) in
            if pred c then find_no (k-1)
            else k in
        let k_lst = find_no len in
        if k_lst = len then t
        else Str (String.sub s 0 len)
    | App (t1,t2,_) ->
        let t2 = do_rec t2 in
        if t2 = empty then do_rec t1
        else append t1 t2 in
    do_rec t

end


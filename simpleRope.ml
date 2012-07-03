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

(**********)
(* Basics *)
(**********)

exception Out_of_bounds

let small_length = 256

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
  when String.length s1 <= small_length && String.length s2 <= small_length ->
    Str (s1^s2)
| App (t1,Str s1,len), Str s2
   when String.length s1 <= small_length && String.length s2 <= small_length ->
     App (t1,Str (s1^s2),len+String.length s2)
| Str s1,App (Str s2,t2,len)
   when String.length s1 <= small_length && String.length s2 <= small_length ->
     App (Str (s1^s2),t2,len+String.length s1)
| _,_ ->
    App (r1,r2,length r1+length r2)


let append r1 r2 = app r1 r2
let append_string r s = app r (of_string s)
and append_char r c = app r (singleton c)

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

let iter_string f s ofs len =
  for k=ofs to len-1 do
    f (String.unsafe_get s k)
  done

let rec iter_rec f i n = function
  | Str s -> iter_string f s i n
  | App (t1,t2,_) ->
      let n1 = length t1 in
      if i+n <= n1 then
        iter_rec f i n t1
      else if i >= n1 then
        iter_rec f (i-n1) n t2
      else begin
        iter_rec f i n1 t1 ;
        iter_rec f (i-n1) (n-n1) t2
      end

let iter_range f t i n =
  if i < 0 || n < 0 || i+n > length t then raise Out_of_bounds ;
  iter_rec f i n t

(**********)
(* Output *)
(**********)

let rec output chan = function
  | Str s -> output_string chan s
  | App (t1,t2,_) -> output chan t1 ; output chan t2

(*************)
(* To string *)
(*************)

let buff = Buffer.create small_length

let rec to_buff = function
  | Str s -> Buffer.add_string buff s
  | App (t1,t2,_) -> to_buff t1 ; to_buff t2

let to_string t =
  Buffer.reset buff ;
  to_buff t ;
  Buffer.contents buff


(*******************)
(* Index functions *)
(*******************)

let rec index_from r i c = match r with
| Str s -> String.index_from s i c
| App (t1,t2,_) ->
    let n1 = length t1 in
    if i < n1 then
      try index_from t1 i c
      with Not_found -> index_from t2 (i-n1) c + n1
    else index_from t2 (i-n1) c

let index r c = index_from r 0 c

let rec rindex_from r i c = match r with
| Str s -> String.rindex_from s i c
| App (t1,t2,_) ->
    let n1 = length t1 in
    if i < n1 then rindex_from t1 i c
    else
      try rindex_from t2 (i-n1) c + n1
      with Not_found -> rindex_from t1 i c

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

(* Attempt: build lexbuff *)
(*
type buff = { rope : t ; pos : int }

let rec blit t src buff dst len = match t with
| Str s -> String.blit s src buff dst len
| App (t1,t2,_) ->
    let n1 = length t1 in
    if src >= n1 then
      blit t2 (src-n1) buff dst len
    if src < len && n1 <= len then
      blit t1 src buff dst len
    else begin
      blit t1 0 buff dst n1 ;
      blit t2 
    end
let fill_buff b s n =


let to_lexbuf t
*)

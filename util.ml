(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: util.ml,v 1.2 2001-05-25 09:20:51 maranget Exp $"            *)
(***********************************************************************)
open Tree
open Htmltext

let rec cost ((k1,k2) as c) = function
  | Text _ | Blanks _ -> c
  | ONode (_,_,ts) -> costs c ts
  | Node (s,ts) ->
      let l1, l2 = Htmltext.cost s in
      costs (l1+k1, l2+k2) ts

and costs k ts = List.fold_left cost k ts

let there s l = List.exists (fun os -> Htmltext.same_style s os) l

let inter s1 s2 =
  List.fold_left
    (fun r s -> if there s s2 then s::r else r)
    [] s1

let sub s1 s2 =
  List.fold_left
    (fun r s -> if there s s2 then r else s::r)
    [] s1

let neutral s =  List.partition Htmltext.blanksNeutral s

let rec is_blank = function
  | Text _ -> false
  | Blanks _ -> true
  | Node (_,ts) | ONode (_,_,ts) -> is_blanks ts

and is_blanks = function
  | [] -> true
  | t::ts -> is_blank t && is_blanks ts

let nodes ss ts = match ss with
| [] -> ts
| _  -> [Node (ss,ts)]

and node ss ts = Node (ss,ts)



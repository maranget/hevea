(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet Moscova, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*  $Id: util.ml,v 1.6 2012-06-05 14:55:39 maranget Exp $c             *)
(***********************************************************************)

open Tree

let rec do_cost ks ((k1,k2) as c) = function
  | Text _ | Blanks _ -> c
  | ONode (_,_,ts) ->
      let c1,c2 = c in
      do_costs ks (1+c1,c2) ts
  | Node (s,ts) ->
      let l1, l2 = ks s in
      do_costs ks (l1+k1, l2+k2) ts

and do_costs ks k ts = List.fold_left (do_cost ks) k ts

let cost ks t = do_cost ks (0,0) t
and costs ks ts = do_costs ks (0,0) ts

let cost_compare  (tags1,fonts1) (tags2, fonts2) =
  if tags1 < tags2 then -1
  else if tags1 > tags2 then 1
  else if fonts1 < fonts2 then -1
  else if fonts1 > fonts2 then 1
  else 0
    


let there s l = List.exists (fun os -> Htmltext.same_style s os) l

let inter s1 s2 =
  List.fold_left
    (fun r s -> if there s s2 then s::r else r)
    [] s1

let sub s1 s2 =
  List.fold_left
    (fun r s -> if there s s2 then r else s::r)
    [] s1

let union s1 s2 =
  List.fold_left
    (fun r s -> if there s r then r else s::r)
    s1 s2


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



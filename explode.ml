open Lexeme
open Htmltext
open Tree

let of_styles env r =
  List.fold_left
    (fun r ss -> match ss with
    | [] -> r
    | _  -> Node (ss,[r]))
    r env

let rec sep = function
  | [] -> []
  | {nat=Other} as s::rem ->
      []::[s]::sep rem
  | s::rem -> match sep rem with
    | [] -> [[s]]
    | ss::rem -> (s::ss)::rem

      
      

let rec tree env t k = match t with
| Text s -> of_styles (sep env) (Text s)::k
| Blanks s ->
    of_styles
      (sep (List.filter (fun s -> not (Htmltext.blanksNeutral s)) env))
      (Blanks s)::
    k
| Node (s,ts) ->
    if s.Tree.tag = A then
      ONode
        (s.Tree.txt, s.Tree.ctxt,
         List.fold_right (tree env) ts [])::k
    else
      let new_env = Htmltext.add_style s env in
      List.fold_right (tree new_env) ts k
| ONode (so,sc,ts) ->
    let ts = List.fold_right (tree env) ts [] in
    ONode (so,sc,ts)::k

let trees ts =  List.fold_right (tree []) ts []

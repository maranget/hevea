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

open Printf

type style = Article | Book

let style = ref Book

let set_style sty = match String.uppercase sty with
| "ARTICLE" -> style := Article
| "BOOK" -> style := Book
| _ ->
    Misc.warning (sprintf "strange style '%s'" sty)


let value_article s = match s with
| "DOCUMENT"|""|"NOW" -> 0
| "PART" -> 1
| "SECTION" -> 2
| "SUBSECTION" -> 3
| "SUBSUBSECTION" -> 4
| "PARAGRAPH" -> 5
| "SUBPARAGRAPH" -> 6
| _         ->
    Misc.warning
      (sprintf "argument '%s' as section level in article mode" s) ;
    7

let value_book s = match s with
| "DOCUMENT"|""|"NOW" -> 0
| "PART" -> 1
| "CHAPTER" ->2
| "SECTION" -> 3
| "SUBSECTION" -> 4
| "SUBSUBSECTION" -> 5
| "PARAGRAPH" -> 6
| "SUBPARAGRAPH" -> 7
| _         ->
    Misc.warning (Printf.sprintf "argument '%s' as section level in book mode" s) ;
    8

let value s =
  (match !style with
  | Article -> value_article
  | Book -> value_book)
    (String.uppercase s)

let pretty_article = function
| 0 -> "document"
| 1 -> "part"
| 2 -> "section"
| 3 -> "subsection"
| 4 -> "subsubsection"
| 5 -> "paragraph"
| 6 -> "subparagraph"
| _ -> assert false

let pretty_book = function
| 0 -> "document"
| 1 -> "part"
| 2 -> "chapter"
| 3 -> "section"
| 4 -> "subsection"
| 5 -> "subsubsection"
| 6 -> "paragraph"
| 7 -> "subparagraph"
| _ -> assert false



let pretty x =
  (match !style with | Article -> pretty_article| Book -> pretty_book) x

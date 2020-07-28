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


open Misc
open Text

exception Error of string


let set_out=Text.set_out;;
let stop = Text.stop;;
let restart = Text.restart;;
let is_empty=Text.is_empty;;

let get_fontsize=Text.get_fontsize;;
let nostyle=Text.nostyle;;
let clearstyle=Text.clearstyle;;
let open_mod=open_mod;;
let erase_mods=Text.erase_mods;;
let has_mod = Text.has_mod;;
let forget_par = Text.forget_par;;
let open_par = Text.open_par;;
let close_par = Text.close_par;;
let par=Text.par;;


let open_block =Text.open_block;;
let close_block =Text.close_block;;
let force_block =Text.force_block;;
let close_flow =Text.close_flow;;
let insert_block =Text.insert_block;;
let insert_attr =Text.insert_attr;;

let open_maths = Text.open_maths
and close_maths = Text.close_maths ;;
let open_display_varg =Text.open_display_varg;;
let open_display =Text.open_display;;
let close_display =Text.close_display;;
let item_display =Text.item_display;;
let force_item_display =Text.force_item_display;;
let erase_display =Text.erase_display
and standard_sup_sub = Text.standard_sup_sub
and limit_sup_sub = Text.limit_sup_sub
and int_sup_sub = Text.int_sup_sub
and addvsize = Text.addvsize
and over = Text.over
and left = Text.left
and right = Text.right
;;
let set_dcount =Text.set_dcount;;
let item = Text.item;;
let nitem = Text.nitem;;
let ditem = Text.ditem;;
let erase_block =Text.erase_block;;
let open_group =Text.open_group;;
let open_aftergroup =Text.open_aftergroup;;
let close_group =Text.close_group;;

let put s = Text.put s
and put_char c = Text.put_char c
and put_unicode i = Text.put_unicode i


let flush_out =Text.flush_out;;
let skip_line =Text.skip_line;;

(* Gestion des references *)
let loc_name=InfoRef.loc_name;;



let open_chan=Text.open_chan;;
let close_chan=Text.close_chan;;
let to_string=Text.to_string;;
let to_style=Text.to_style;;
let get_current_output =Text.get_current_output;;

(* Finalisation du fichier info *)
let finalize check =
  if check then begin
    if !verbose>1 then prerr_endline "Beginning of second phase.";
    InfoRef.finalize_nodes ();
    Text.finalize check ;
    let name,buf =
      if Parse_opts.filter then
        let texte = get_current_output () in 
        "",MyLexing.from_string texte
      else
      (* changer de nom de fichier (renommer ?) *)
        try
          let f = Parse_opts.name_out^".tmp" in
          f,Lexing.from_channel  (open_in f)
        with Sys_error msg ->
          Misc.fatal ("Cannot re-open info output file "^msg)
    in
    InfoRef.dump buf ;
    if not Parse_opts.filter && !verbose <= 0 then Mysys.remove name
  end else
    Text.finalize false
;;

let horizontal_line =Text.horizontal_line;;
let put_separator =Text.put_separator;;
let unskip = Text.unskip;;
let put_tag =Text.put_tag;;
let put_hspace =Text.put_hspace;;
let put_open_group =Text.put_open_group;;
let put_close_group =Text.put_close_group;;
let put_in_math =Text.put_in_math;;


let open_table =Text.open_table;;
let new_row =Text.new_row;;
let open_cell =Text.open_cell;;
let erase_cell =Text.erase_cell;;
let close_cell =Text.close_cell;;
let do_close_cell = Text.do_close_cell;;
let open_cell_group = Text.open_cell_group;;
let close_cell_group = Text.close_cell_group;;
let erase_cell_group = Text.erase_cell_group;;
let close_row =Text.close_row;;
let erase_row =Text.erase_row;;
let close_table =Text.close_table;;
let make_border = Text.make_border;;
let make_inside = Text.make_inside;;
let make_hline = Text.make_hline;;

let infonode = InfoRef.infonode;;
let infoextranode = InfoRef.infoextranode;;
let infomenu = InfoRef.infomenu;;

let image = Text.image;;

type saved = Text.saved

let check = Text.check
and hot = Text.hot


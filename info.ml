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


let header = "$Id: info.ml,v 1.6 1999-05-17 15:52:47 tessaud Exp $"

open Misc
open Text
open InfoRef

exception Error of string
exception Close of string


let iso =Text.iso;;
let set_out=Text.set_out;;
let get_last_closed=Text.get_last_closed;;
let set_last_closed=Text.set_last_closed;;
let is_empty=Text.is_empty;;

let get_fontsize=Text.get_fontsize;;
let nostyle=Text.nostyle;;
let clearstyle=Text.clearstyle;;
let open_mod=open_mod;;
let erase_mods=Text.erase_mods;;
let open_mods =Text.open_mods;;
let close_mods=Text.close_mods;;
let par=Text.par;;
let forget_par =Text.forget_par;;


let open_block =Text.open_block;;
let close_flow =Text.close_flow;;
let close_block =Text.close_block;;
let force_block =Text.force_block;;
let insert_block =Text.insert_block;;

let open_display =Text.open_display;;
let close_display =Text.close_display;;
let item_display =Text.item_display;;
let force_item_display =Text.force_item_display;;
let end_item_display =Text.end_item_display;;
let begin_item_display =Text.begin_item_display;;
let erase_display =Text.erase_display;;

let set_dt =Text.set_dt;;
let set_dcount =Text.set_dcount;;
let item =Text.item;;
let change_block =Text.change_block;;
let erase_block =Text.erase_block;;
let open_group =Text.open_group;;
let open_aftergroup =Text.open_aftergroup;;
let close_group =Text.close_group;;

let size = ref 0 ;;
let max_size = 50000;;

let put s =
  size:=!size + String.length s;
  if !size > max_size then begin
    if InfoRef.change_file() then
      size := 0;
  end;
  Text.put s;;

let put_char c=
  size :=!size +1;
  if !size > max_size then begin
    if InfoRef.change_file() then
      size := 0;
  end;
  Text.put_char c;;

let flush_out =Text.flush_out;;
let skip_line =Text.skip_line;;


let delay =Text.delay;;
let flush =Text.flush;;
let forget =Text.forget;;


(* Gestion des references *)
(*
let loc_ref=InfoRef.loc_ref;;
*)
let loc_ref s1 s2 = (* fait la reference *)
  put ("\\@reference{"^s2^"}");
;;


let loc_name=InfoRef.loc_name;;



let insert_vdisplay=Text.insert_vdisplay;;
let freeze =Text.freeze;;
let open_chan=Text.open_chan;;
let close_chan=Text.close_chan;;
let to_string=Text.to_string;;
let to_style=Text.to_style;;
let get_current_output =Text.get_current_output;;

(* Finalisation du fichier info *)
let finalize check =
  if !verbose>1 then prerr_endline "Beginning of second phase.";
  let buf, out_chan = match Parse_opts.name_out with
  | "" -> 
      let texte = get_current_output () in 
      Text.finalize check;
      Lexing.from_string texte, Out.create_chan stdout
  | s ->  
      Text.finalize check;
      (* changer de nom de fichier (renommer ?) *)
      let f = s^".tmp" in
      if !verbose >1 then prerr_endline ("Out file:"^s);
      Lexing.from_channel  (open_in f),Out.create_chan (open_out s)
  in
  InfoRef.finalize_nodes ();
  InfoRef.set_out out_chan;
  InfoRef.set_out_file Parse_opts.name_out;
  InfoRef.main buf;
  (* deuxieme passe sur le fichier *)
  if Parse_opts.name_out <> "" then Sys.remove (Parse_opts.name_out^".tmp");
;;

let horizontal_line =Text.horizontal_line;;
let put_separator =Text.put_separator;;
let unskip = Text.unskip;;
let put_tag =Text.put_tag;;
let put_nbsp =Text.put_nbsp;;
let put_open_group =Text.put_open_group;;
let put_close_group =Text.put_close_group;;
let put_in_math =Text.put_in_math;;


let open_table =Text.open_table;;
let new_row =Text.new_row;;
let open_cell =Text.open_cell;;
let erase_cell =Text.erase_cell;;
let close_cell =Text.close_cell;;
let do_close_cell = Text.do_close_cell;;
let close_row =Text.close_row;;
let erase_row =Text.erase_row;;
let close_table =Text.close_table;;


let infonode = InfoRef.infonode;;
let infomenu = InfoRef.infomenu;;

let image = Text.image;;

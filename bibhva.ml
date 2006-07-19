(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*  Luc Maranget, projet MOSCOVA, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

(*  $Id: bibhva.ml,v 1.2 2006-07-19 16:17:13 maranget Exp $    *)

let parse_args () =
  let options = ref []
  and name = ref "" in
  for k = 1 to Array.length Sys.argv-1 do
    let a = Sys.argv.(k) in
    if String.length a > 0 && a.[0] == '-' then
      options := !options @ [a]
    else
      name := a
  done ;
  !options, !name


exception Error of string

let rename name1 name2 =
  try Sys.rename name1 name2
  with Sys_error msg ->
    raise (Error (Printf.sprintf "rename %s %s: %s" name1 name2 msg))

let remove name =
  try Sys.remove name
  with Sys_error msg ->
    raise (Error (Printf.sprintf "remove %s: %s" name msg))

let file_exists name =
  try Sys.file_exists name
  with Sys_error msg ->
    raise (Error (Printf.sprintf "file_exists %s: %s" name msg))
  
let preserved = ref []

let preserve x = preserved := x :: !preserved    

(* Not 100% safe, but will do most of the time *)
let rec temp_file name suff =
  let temp_name = name ^ suff in
  if file_exists temp_name then
    temp_file temp_name suff
  else
    temp_name

let save_to_temp file_name =
  if file_exists file_name then begin
    let tmp_name = temp_file file_name "~" in
    rename file_name tmp_name ;
    preserve (file_name, Some tmp_name)
  end else begin
    preserve  (file_name, None)
  end

and restore () =
  let restore_one x =
    try begin match x with
    | name,None ->
        if file_exists name then remove name
    | name,Some tmp_name ->
        if file_exists name then remove name ;
        rename tmp_name name
    end with
      Error msg -> Printf.eprintf "Warning: %s\n" msg in
  List.iter restore_one !preserved ;
  preserved := []
          
let run_bibtex options name =
  try
  let base =
    if Filename.check_suffix name ".haux" then
      Filename.chop_suffix name ".haux"
    else
      name in
  let name_aux = base ^ ".aux"
  and name_haux = base ^ ".haux" in
  save_to_temp name_aux ;
  rename name_haux name_aux ; preserve (name_haux, Some name_aux) ;
  let cmd = "bibtex "^String.concat " " (options @ [name_aux]) in
  let name_bbl  = base ^ ".bbl" in
  save_to_temp name_bbl ;
  (* bibtex fails too easily to account for its status code *)
  ignore (Sys.command cmd) ;
  let name_hbbl = base ^ ".hbbl" in
  if file_exists name_hbbl then remove name_hbbl ;
  rename name_bbl name_hbbl ;
  restore ()
  with
  | Error msg ->
      Printf.eprintf "Bibtex run failed: %s\n" msg ;
      restore () ;
      exit 2
  | e ->
      restore () ;
      raise e

let _ =
  let options, name = parse_args () in
  run_bibtex options name ;
  exit 0

  


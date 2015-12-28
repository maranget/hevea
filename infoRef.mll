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

{

open Lexing
open Misc

let compat_mem tbl key =
  try let _ = Hashtbl.find tbl key in true with Not_found -> false
;;


exception Error of string

type node_t = {
    mutable name : string;
    mutable comment : string;
    mutable previous : node_t option;
    mutable next : node_t option;
    mutable up : node_t option;
    mutable pos : int;
  } 
;;

type menu_t = {
    mutable num : int;
    mutable nom : string;
    mutable nod : node_t option;
    mutable nodes : node_t list;
  }
;;


let menu_list = ref [];;

let nodes = Hashtbl.create 17;;
let delayed  = ref [];;
let current_node = ref None;;

let menu_num = ref 0
;;

let counter = ref 0
and pos_file = ref 0
;;

let abs_pos () = !counter + !pos_file
;;


let cur_file = ref (Parse_opts.name_out)
;;

let file_number = ref 1
;;

type label_t = {
    mutable lab_name : string;
    mutable noeud : node_t option;
  };;

let labels_list = ref [];;

let files = ref [];;
let top_node = ref false;;

let hot_start () =
  menu_list :=  [];
  Hashtbl.clear nodes ;
  current_node := None ;
  menu_num := 0 ;
  counter := 0 ;
  pos_file := 0 ;
  cur_file := Parse_opts.name_out ;
  files := [] ;
  top_node := false ;
  file_number :=  1 ;
  labels_list := []
;;

let infomenu arg =
  menu_num:=!menu_num+1;
  menu_list := { 
    num = !menu_num;
    nom = arg;
    nod = !current_node;
    nodes = [];
  } ::!menu_list;
  Text.open_block "INFOLINE" "";
  Text.put ("\\@menu"^string_of_int !menu_num^"\n");
  Text.close_block "INFOLINE"
;;

let rec cherche_menu m = function
  | [] -> raise (Error ("Menu ``"^m^"'' not found"))
  | menu::r -> 
      if menu.nom = m then menu
      else cherche_menu m r
;;

let rec cherche_menu_par_num n = function
  | [] -> raise (Error ("Menu not found"))
  | menu::r -> 
      if menu.num = n then menu
      else cherche_menu_par_num n r
;; 

let ajoute_node_dans_menu n m =
  try
  let menu = cherche_menu m !menu_list in
  menu.nodes <- n :: menu.nodes;
  menu.nod
  with _ -> None
;;

let verifie name =
  Misc.string_map
    (fun c -> match c  with
    | '\t'
    | ',' 
    | '\n' -> ' '
    | '.' -> '-'
    |  _ -> c)
    name
;;




(* References *)

let rec cherche_label s = function
  | [] -> raise Not_found
  | l::r -> if l.lab_name=s then l.noeud else cherche_label s r
;;

let rec change_label s = function
  |  [] -> Misc.warning ("Cannot change label: ``"^s^"''")
  | l::r ->
      if l.lab_name = s then
        l.noeud <- !current_node 
      else
        change_label s r

let loc_name s1 = (* pose un label *)
  let _ = 
    try 
      let _ = cherche_label s1 !labels_list in
      Misc.warning ("Multiple use of label: "^s1)
    with Not_found -> ()
  in

  let l = {
    lab_name = s1;
    noeud = !current_node ;
  } in

  labels_list := l:: !labels_list;
  Text.open_block "INFO" "" ;
  Text.put "\\@name{" ;
  Text.put s1 ;
  Text.put "}" ;
  Text.close_block "INFO" ;
  if !verbose > 1 then prerr_endline ("InfoRef.loc_name, label="^s1);
;;



(* Sortie du fichier final *)

let out_cur = ref (Out.create_null ())
;;

let set_out chan =
  if !verbose >3 then prerr_endline "Set_out";
  out_cur := chan
;;

let set_out_file s =
  if !verbose >3 then prerr_endline ("Set_out_file :"^s);
  cur_file := s
;;

let put s = 
  if !verbose >3 then
    prerr_endline ("put :"^s);
  counter:=!counter + String.length s;
  Out.put !out_cur s
;;

let put_char c =
  if !verbose >3 then
    prerr_endline ("put_char :"^String.make 1 c);
  counter:=!counter +1;
  Out.put_char !out_cur c
;;

let put_credits () =
  put "\n\n-------------------------------------\nThis file has been translated from LaTeX by HeVeA.\n\n";

and put_header () =
  put "This file has been translated from LaTeX by HeVeA.\n"
;;

let next_file () = 
  Out.close !out_cur ;
  file_number := !file_number +1;
  cur_file := Parse_opts.name_out ^ "-" ^ string_of_int !file_number ;
  if !verbose > 0 then
    prerr_endline ("Change file to "^ !cur_file) ;
  set_out (Out.create_chan (open_out !cur_file)) ;
  files := (!cur_file,abs_pos ()) :: !files ;
  pos_file := abs_pos () ;
  put_header () ;
  counter := 0

;;




let noeud_name n = n.name
;;


let affiche_menu num =
  let menu = cherche_menu_par_num num !menu_list in
  if menu.nodes <> [] then begin
    put "* Menu:\n\n";
    let rec affiche_items = function
      | [] -> ()
      | n::reste ->
	  put ("* "^noeud_name n^"::\t"^n.comment^"\n");
	  affiche_items reste;
    in
    affiche_items (List.rev menu.nodes);
    if !verbose >1 then
      prerr_endline ("Menu :"^menu.nom);
  end
;;
    

let put_node n = 
  put ("Node: "^noeud_name n^""^string_of_int n.pos^"\n")
;;

let  do_affiche_tag_table s = 
  put ("\n\nTag Table:\n"^(if s<> "" then s^"\n" else "")) ;  
  Hashtbl.iter
    (fun _ n -> match n.name with | "Top" -> put_node n | _ -> ()) nodes ;
  Hashtbl.iter
    (fun _ n -> match n.name with | "Top" -> ()| _ ->  put_node n) nodes;
  put "\nEnd Tag Table\n";
;;


let affiche_tag_table ()=
  match !files with
  | [_] ->
    do_affiche_tag_table ""
  | _   ->
    let rec do_indirect = function
      | [] -> ()
      | (f,p)::reste ->
          put (f^": "^string_of_int p^"\n");
	  do_indirect reste
    in
    Out.close !out_cur ;
    set_out (Out.create_chan (open_out Parse_opts.name_out)) ;
    put_header () ;
    put "\nIndirect:\n";
    do_indirect (List.rev !files);
    do_affiche_tag_table "(Indirect)"
;;


let affiche_node nom =
  if !top_node then begin
    put_credits () ;
    top_node := false
  end ;
  let noeud = 
    try Hashtbl.find nodes nom
    with Not_found ->  raise (Error ("Node not found :"^nom))
  in
  if not Parse_opts.filter && !counter > 50000 then begin
    next_file ()
  end;
  noeud.pos <- abs_pos ();
  put "\n";
  put ("File: "^Parse_opts.base_out^", Node: "^noeud_name noeud);
  (match noeud.next with
  | None -> ()
  | Some n -> put (",\tNext: "^noeud_name n));
  (match noeud.previous with
  | None -> ()
  | Some n -> put (",\tPrev: "^noeud_name n));
  (match noeud.up with
  | None ->
      if noeud.name = "Top" then begin
        put ",\tUp: (dir)" ;
        top_node := true
      end else
	put ",\tUp: Top"
  | Some n -> put (",\tUp: "^noeud_name n));
  put_char '\n';
  if !verbose >1 then
    prerr_endline ("Node : "^noeud_name noeud);
  
;;

let affiche_ref key =
  try
    let l =  cherche_label key !labels_list in
    match l with
    | None -> ()
    | Some node -> put ("*Note "^noeud_name node^"::")
  with
  | Not_found -> () (* A warning has already been given *)
;;

let footNote_label = ref ""
;;

} 


rule main = parse
| "\\@menu"
    {
  let num = numero lexbuf in
  affiche_menu num;
  main lexbuf}
| "\\@node"
    {
  let nom = finitLigne lexbuf in
  affiche_node nom;
  main lexbuf}
| "\\@reference{"
    {
  let key = arg lexbuf in
  affiche_ref key;
  main lexbuf}
| "\\@name{"
  {let _ = arg lexbuf in
  main lexbuf}
| eof
    {affiche_tag_table ()}

| _ 
    {let lxm = lexeme_char lexbuf 0 in
    put_char lxm;
    main lexbuf}

and numero = parse
    ['0'-'9']+
    {let lxm = lexeme lexbuf in
    int_of_string lxm}
| _ {raise (Error "Syntax error in info temp file")}
    
and finitLigne = parse
    [^'\n']+'\n'
    {let lxm = lexeme lexbuf in
    String.sub lxm 0 ((String.length lxm) -1)}
| _ {raise ( Error "Syntax error in info temp file: no node name.")}
    
and arg = parse
    [^'}']+'}'
    {let lxm= lexeme lexbuf in
    String.sub lxm 0 ((String.length lxm) -1)}
| _ {raise (Error "Syntax error in info temporary file: invalid reference.")}
    
and labels = parse
| "\\@name{"
    {let key = arg lexbuf in
    key::labels lexbuf}
| _ {labels lexbuf}
| eof {[]}


{
let do_infonode opt num arg = 

  let n = {
    name = verifie num;
    comment = arg;
    previous = None;
    next = None;
    up = None;
    pos = 0;
  } in
  if compat_mem nodes n.name then
    raise (Error ("Duplicate node name: "^n.name));  
  n.up <- (match opt with
    "" -> None
  | m ->  ajoute_node_dans_menu n m);
  Hashtbl.add nodes n.name n;
  Text.open_block "INFOLINE" "";
  Text.put ("\\@node"^n.name^"\n");
  Text.close_block "INFOLINE";
  current_node := Some n;
  if !verbose>1 then prerr_endline ("Node added :"^n.name^", "^n.comment)

let infoextranode num nom text =
  delayed := (num,nom,text) :: !delayed

and flushextranodes () =
  let rec flush_rec = function
    | [] -> ()
    | (num,nom,text) :: rest ->
        do_infonode "" num nom ;
        Text.open_block "INFO" "" ;
        Text.put text ;
        Text.close_block "INFO" ;
        let labs = labels (MyLexing.from_string text) in
        List.iter (fun lab -> change_label lab !labels_list) labs ;
        flush_rec rest in
  flush_rec !delayed ;
  delayed := [] 
;;

let infonode opt num arg =
  flushextranodes () ;
  do_infonode opt num arg


(* finalisation des liens entre les noeuds *)
let rec do_finalize_nodes suivant = function
  | [] -> ()
  | n::reste -> 
      if !verbose>2 then prerr_endline ("node :"^n.name);
      n.next <- suivant;
      (match  suivant with
      |	None -> ()
      |	Some suiv -> suiv.previous <- Some n );
      do_finalize_nodes (Some n) reste
;;

let rec do_finalize_menus = function
  | [] -> ()
  | m::reste ->
      if m.nodes <> [] then begin
	do_finalize_nodes None m.nodes;
	(match m.nod with
	    None -> ()
	  | Some n -> if n.name = "Top" then
	      let first_node = List.hd (List.rev m.nodes) in
	      n.next <- Some first_node;
	      first_node.previous <- Some n;
	       (* On descend dans l'arborescence des menus *)
	      let last_node = List.hd m.nodes in
	      match last_node.next with
		| None -> ()
		| Some suiv -> suiv.previous <- Some n;
        (* On remonte les menus au meme niveau *)
	);
	do_finalize_menus reste;
      end
;;

let finalize_nodes () =
  if !verbose>2 then prerr_endline "finalizing nodes";
  flushextranodes () ;
  do_finalize_menus (List.rev !menu_list);
  if !verbose>2 then prerr_endline "finalizing done.";
;;

let dump buff =
  let name,out_chan = match Parse_opts.name_out with
  | ""|"-" -> "", Out.create_chan stdout
  | s  ->
      let name = s^"-1" in
      name, Out.create_chan (open_out name) in  
  if !verbose > 0 then
    prerr_endline ("Final dump in "^name) ;
  set_out out_chan ;
  set_out_file name ;
  put_header () ;
  files := [name,abs_pos ()] ;
  main buff ;
  Out.close !out_cur ;
  if !file_number = 1 then
    Mysys.rename !cur_file Parse_opts.name_out
}





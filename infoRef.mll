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
let header = "$Id: infoRef.mll,v 1.10 1999-06-18 15:09:04 tessaud Exp $"
;;


open Lexing
open Misc


exception Error of string

type node_t = {
    mutable name : string;
    mutable comment : string;
    mutable file : string;
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

let current_node = ref None;;

let menu_num = ref 0
;;

let counter = ref 0
;;
let cur_file = ref (Parse_opts.name_out^"-1")
;;
let file_number = ref 1
;;

let infomenu arg =
  menu_num:=!menu_num+1;
  menu_list := { 
    num = !menu_num;
    nom = arg;
    nod = !current_node;
    nodes = [];
  } ::!menu_list;
  Text.open_block "INFO" "";
  Text.put ("\n\\@menu"^string_of_int !menu_num^"\n");
  Text.close_block "INFO"
;;

let rec cherche_menu m = function
  | [] -> raise (Error ("Menu not found"))
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
  let menu = cherche_menu m !menu_list in
  menu.nodes <- n :: menu.nodes;
  menu.nod
;;


let verifie name =
  let nom = String.copy name in
  for i = 0 to String.length name -1 do
    match nom.[i] with
    | '\t' -> nom.[i] <- ' '
    | ',' -> nom.[i] <- ' '
    | '.' -> nom.[i] <- '-'
    | '\n' -> nom.[i] <- ' '
    |  _ -> ()
  done;
  nom
;;

let infonode opt num arg = 

  let n = {
    name = verifie num;
    comment = arg;
    file = !cur_file;
    previous = None;
    next = None;
    up = None;
    pos = 0;
  } in
  if Hashtbl.mem nodes n.name then raise (Error ("Duplicate node name :"^n.name));  
  n.up <- (match opt with
    "" -> None
  | m ->  ajoute_node_dans_menu n m);
  Hashtbl.add nodes n.name n;
  Text.open_block "INFO" "";
  Text.put ("\n\\@node"^n.name^"\n");
  Text.close_block "INFO";
  current_node := Some n;
  if !verbose>1 then prerr_endline ("Node added :"^n.name^", "^n.comment);
;;

let change_file() = 
  let changed = 
    match !current_node with
      Some n -> if n.file = !cur_file then begin
	file_number := !file_number +1;
	true;
      end else false
    | _ -> false
  in
  
  if Parse_opts.name_out <> "" then
    cur_file := Parse_opts.name_out ^ "-" ^ string_of_int !file_number;
  changed
;;

(* References *)
type label_t = {
    mutable lab_name : string;
    mutable noeud : node_t option;
  };;

let labels_list = ref [];;

let rec cherche_label s = function
  | [] -> raise Not_found
  | l::r -> if l.lab_name=s then l.noeud else cherche_label s r
;;

let loc_name s1 s2 = (* pose un label *)
  let _ = 
    try 
      let _ = cherche_label s1 !labels_list in
      Parse_opts.warning ("Duplicate use of labels: "^s1)
    with Not_found -> ()
  in

  let l = {
    lab_name = s1;
    noeud = !current_node ;
  } in

  labels_list := l:: !labels_list;
  if !verbose > 1 then prerr_endline ("InfoRef.loc_name, label="^s1);
;;

let loc_ref s1 s2 = (* fait la reference *)
  Text.put ("\\@reference{"^s2^"}");
;;



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
	do_finalize_nodes 
	  (match m.nod with
	    None -> None
	  | Some n -> n.next)
	  m.nodes;
	(match m.nod with
	  None -> ()
	|	 Some n -> 
	    let first_node = List.hd (List.rev m.nodes) in
	    n.next <- Some first_node;
	    first_node.previous <- Some n;
	  (* On descend dans l'arborescence des menus *)
	    let last_node = List.hd m.nodes in
	    (match last_node.next with
	    | None -> ()
	    | Some suiv -> suiv.previous <- Some n);
          (* On remonte les menus au meme niveau *)
	  );
	do_finalize_menus reste;
      end
;;

let finalize_nodes () =
  if !verbose>2 then prerr_endline "finalizing nodes";
  do_finalize_menus (List.rev !menu_list);
  if !verbose>2 then prerr_endline "finalizing done.";
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

let files = ref [];;
let top_node = ref false;;

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
;;

let noeud_name n = n.name
;;

let noeud_file_name n =
  (if !cur_file = n.file 
  then ""
  else "("^n.file^")")
  ^n.name
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
    

let  affiche_tag_table_un out_file = 
  if !top_node then begin
    put_credits ();
    top_node := false;
  end;

  put "\n\nTag table:\n";
  Hashtbl.iter (fun nom n -> put ("File: "^ out_file ^",\tNode: "^noeud_name n^""^string_of_int n.pos^"\n")) nodes;
  put "\nEnd tag table\n";
;;


let affiche_tag_table ()=
  set_out (Out.create_chan (open_out Parse_opts.name_out));
  set_out_file Parse_opts.name_out;
  put_credits ();
  let rec do_indirect = function
    | [] -> ()
    | (f,p)::reste -> put (f^": "^string_of_int p^"\n");
	do_indirect reste
  in
  put "\n\nIndirect:\n";
  do_indirect (List.rev !files);
  put "\n\nTag table:\n(Indirect)\n";
  Hashtbl.iter (fun nom n ->put ("File: "^n.file^",\tNode: "^noeud_name n^""^string_of_int n.pos^"\n")) nodes;
  put "\nEnd tag table\n";
  Out.close !out_cur;
;;


let affiche_node nom =
  if !top_node then begin
    put_credits ();
    top_node := false;
  end;
  let noeud = 
    try Hashtbl.find nodes nom
    with Not_found ->  raise (Error ("Node not found :"^nom))
  in
  if noeud.file <> !cur_file then begin
    Out.close !out_cur;
    set_out_file noeud.file;
    set_out (Out.create_chan (open_out noeud.file));
    put_credits ();
    put_char '\n';
    files := (!cur_file,!counter) :: !files;
  end;
  noeud.pos <- !counter;
  put "\n";
  (match noeud.file with
    "" -> ()
  | f -> put ("File: "^f^",\t"));
  put ("Node: "^noeud_name noeud);
  (match noeud.next with
  | None -> ()
  | Some n -> put (",\tNext: "^noeud_name n));
  (match noeud.previous with
  | None -> ()
  | Some n -> put (",\tPrev: "^noeud_name n));
  (match noeud.up with
  | None -> if noeud.name = "Top" then put ",\tUp: (dir)."
  | Some n -> put (",\tUp: "^noeud_name n));
  put_char '\n';
  if noeud.name="Top" then top_node := true;

  if !verbose >1 then
    prerr_endline ("Node : "^noeud_name noeud);
;;

let affiche_ref key =
  let l = 
    try
      cherche_label key !labels_list;
    with Not_found ->  raise (Error ("Reference to no label :"^key));
  in
  match l with
    | None -> ()
    | Some node -> put ("*Note "^noeud_name node^"::")
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
| "@@footNote{"
    {
  footNote_label := arg lexbuf;
  put "* Note ";
  foot lexbuf }
| eof
    {
  if List.length !files = 1 then begin
    affiche_tag_table_un Parse_opts.name_out;
    Out.close !out_cur;
    Sys.rename !cur_file Parse_opts.name_out;
  end else begin
    Out.close !out_cur;
    affiche_tag_table ();
  end
    }
| _ 
    {let lxm = lexeme lexbuf in
    put lxm;
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
    
and foot = parse
    "@@footNoteEnd"
    {
  put ":";
  let l = 
    try
      cherche_label !footNote_label !labels_list;
    with Not_found ->  raise (Error ("Reference to no label :"^ !footNote_label));
  in
  (
  match l with
  | None -> ()
  | Some node -> put (noeud_name node^".")
	);  
  main lexbuf
} 
| eof
    { raise (Error "End of file in footnote declaration")}
| _ 
    {
  let lxm = lexeme lexbuf in
  put lxm;
  foot lexbuf}

{}




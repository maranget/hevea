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
open Entry


let missing_index tag =
  Misc.warning
    ("Index structure not found, missing "^
    (match tag with
    | "default" -> "\\makeindex"
    | _  -> "\\newindex{"^tag^"}.."))
;;


type entry_t = {key : key ; see : string option ; item : string}
;;

type entry =
  | Good of entry_t
  | Bad

let first_key = function 
  | (x::_),_ -> x
  | _ -> raise (Misc.Fatal ("Empty key in first_key"))

let pretty_key (l,p) =
 let rec p_rec l p = match l,p with
   [],[] -> ""
 | [x],[""]-> x
 | [x],[y]-> x^"@"^y
 | x::xs,""::ys -> x^"!"^p_rec xs ys
 | x::xs,y::ys -> x^"@"^y^"!"^p_rec xs ys
 |  _,_ -> assert false in
 p_rec l p
;;


type t_index =
  {mutable name : string ;
  mutable onebad : bool ;
  mutable counter : int ;
  sufin : string ; sufout : string ;
  from_file : entry array option ;
  from_doc : entry Table.t ;
  out : Out.t}

let itable = Hashtbl.create 17
;;


let read_index_file name file =
  let lexbuf = Lexing.from_channel file in
  let r = Table.create Bad in
  let rec do_rec () =
    try
      let arg1,arg2 = read_indexentry lexbuf in
      let entry =
        try
          let k,see = read_key (MyLexing.from_string arg1) in
          Good {key=k ; see=see ; item = arg2}
        with Entry.NoGood ->
          Misc.warning
          ("Bad index arg syntax in file: "^name^
           ", index entry is '"^arg1^"'") ;
          Bad in
      Table.emit r entry ;
      do_rec ()
    with
    | Entry.Fini -> Table.trim r in

  let r = do_rec () in
  if !verbose > 0 then
    prerr_endline ("Index file: "^name^" succesfully read");
  Some r

let find_index tag = Hashtbl.find itable tag

let changename tag name =
  try
    let idx = find_index  tag in
    idx.name <- name
  with Not_found -> missing_index tag

let index_lbl tag i = "hevea_"^tag^string_of_int i
let index_filename suff = Parse_opts.base_out^".h"^suff

let do_treat tag arg refvalue anchor =
(*  prerr_endline ("Index treat: "^tag^", "^arg^", "^refvalue) ; *)
  try
    if !verbose > 2 then prerr_endline ("Index.treat with arg: "^arg) ;
    let {from_doc = from_doc ; out = out} as idx =  find_index tag in
    let lbl =
      match anchor with
      | Some lbl -> lbl
      | None     -> index_lbl tag idx.counter in    
    let refvalue = match refvalue with "" -> "??" | s -> s in
    let item = "\\@locref{"^lbl^"}{"^refvalue^"}" in
    Out.put out "\\indexentry{" ;
    Out.put out arg ;
    Out.put out "}{" ;
    Out.put out item ;
    Out.put out "}\n" ;
    
    let lexbuf = MyLexing.from_string arg in
    let entry =
      try
        let key,see = read_key lexbuf in
        Good {key = key ; see = see ; item = item}
      with
      | Entry.NoGood ->
          idx.onebad <- true ;
          Misc.warning ("Bad index syntax: '"^arg^"'") ;
          Bad in
    Table.emit from_doc entry ;
    idx.counter <- idx.counter + 1 ;
    lbl
  with
  | Not_found -> missing_index tag ; ""
;;

let treat tag arg refvalue =
  do_treat tag arg refvalue None

and treat_anchor tag arg refvalue lbl =
  ignore (do_treat tag arg refvalue (Some lbl))

(* Compare function for keys *)


let is_alpha c =  ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

let compare_char c1 c2 =
  if is_alpha c1 && is_alpha c2 then
    let r = compare (Char.uppercase c1) (Char.uppercase c2) in
    if r <> 0 then r
    else compare c1 c2
  else if is_alpha c1 then 1
  else if is_alpha c2 then -1
  else compare c1 c2

exception Result of int

let compare_string s1 s2 =
  let i = ref 0
  and l1 = String.length s1
  and l2 = String.length s2 in
  begin try
    while true do
      begin if !i >= l1 then
        if !i >= l2 then raise (Result 0)
        else raise (Result (-1))
      else if !i >= l2 then raise (Result 1)
      else
        let c = compare_char s1.[!i] s2.[!i] in
        if c <> 0 then raise (Result c)
      end ;
      i := !i + 1
    done ;
    0
  with Result x -> x
  end
   
let comp (l1,p1) (l2,p2) =

  let rec c_rec l1 l2 p1 p2 = match l1,l2 with
  | [],[] -> 0    
  | [],_  -> -1
  | _,[]  -> 1
  | x1::r1,x2::r2 ->
      let t = compare_string x1 x2 in
      if t<> 0 then t
      else begin
        match p1,p2 with
        | y1::p1, y2::p2 ->
            let t = compare_string y1 y2 in
            if t <> 0 then t
            else
              c_rec r1 r2 p1 p2
        | _,_ -> assert false
      end in
  c_rec l1 l2 p1 p2
;;

module OrderedKey = struct
  type t = key
  let compare = comp
end
;;

module KeySet =  Set.Make(OrderedKey)
;;


let rec common e1 e2 = match e1,e2 with
  ([],_),_        -> e1,e2
| _,([],_)        -> e1,e2
| ([_],_),([_],_) -> e1,e2
| (_::_,_),([_],_) -> e1,e2
| (x1::r1,_::p1),(x2::r2,_::p2) ->
    if x1=x2 then
      common (r1,p1) (r2,p2)
    else
      e1,e2
|  _ -> assert false
;;

let rec close_prev out = function
  [],_ | [_],_ -> ()
| _::r,_::p    ->
    Out.put out "\\end{indexenv}\n" ;
    close_prev out (r,p)
|  _ -> assert false
;;

let rec open_this out k = match k with
  [],_ -> ()
| k::r,p::rp ->
    Out.put out "\\indexitem " ;
    let tag = if p <> "" then p else k in
    Out.put out tag  ;
    begin match r with
      [] -> ()
    | _  -> Out.put out "\\begin{indexenv}\n" ;
    end ;
    open_this out (r,rp)
|  _ -> assert false
;;

let start_change s1 s2 = match s1,s2 with
| "",_ -> false
| _,"" -> false
| _,_  -> Char.uppercase s1.[0] <> Char.uppercase s2.[0]

let print_entry out _ entries bk k xs  =
  let rp,rt = common bk k in
  close_prev out rp ;
  if fst rp = [] then
    Out.put out "\\begin{indexenv}\n"
  else begin
    let top_prev = first_key bk
    and top_now = first_key k in
    if start_change top_prev top_now then
      Out.put out "\\indexspace\n"
  end ;
  open_this out rt ;  

  let rec prints = function
    [] -> Out.put_char out '\n'
  | i::r ->
      Out.put out ", " ;
      begin match entries.(i) with
      | Good e ->
          begin match e.see with
          | None     ->  Out.put out e.item
          | Some see ->  Out.put out ("\\"^see^"{"^e.item^"}")
          end ;
      | Bad -> ()
      end ;
      prints r in

   prints (List.rev xs)
;;

     
let make_index t =
  let table = Hashtbl.create 17
  and all = ref KeySet.empty in
  for i = 0 to Array.length t - 1 do
    match t.(i) with
    | Good e ->
        all := KeySet.add e.key !all ;
        Hashtbl.add table e.key i
    | Bad -> ()
  done ;
  !all,table


let output_index tag entries out =
  if !verbose > 1 then prerr_endline ("Print index ``"^tag^"''") ;
  let all_keys,table = make_index entries in
  let prev = ref ([],[]) in
  KeySet.iter (fun k ->
    if !verbose > 2 then
      prerr_endline ("Print_entry: "^pretty_key k);
    print_entry out tag entries !prev k (Hashtbl.find_all table k) ;
    prev := k)
    all_keys ;
  let pk,_ = !prev in
  List.iter (fun _ -> Out.put out "\\end{indexenv}\n") pk


let create_hind t tag sufout = 
  let outname = index_filename sufout in
  try 
    let chan = open_out outname in
    output_index tag t (Out.create_chan chan) ;
    close_out chan
  with
  | Sys_error s ->
      Misc.warning ("File error for "^outname^": "^s)

let newindex tag sufin sufout name =  
(*  prerr_endline ("New index: "^tag) ; *)
  Hashtbl.remove itable tag ;
  let from_file =
    try
      let filename = index_filename sufin in
      let file = open_in filename in
      read_index_file filename file
    with Sys_error _ -> None in
  begin match from_file with
  | None -> ()
  | Some t -> create_hind t tag sufout
  end ;
  Hashtbl.add itable tag
    {name = name ;
    onebad = false ;
    counter = 0 ;
    sufin = sufin ; sufout = sufout ;
    from_file = from_file ;
    from_doc = Table.create Bad ;
    out = Out.create_buff ()}

let print main tag =
  try
    let idx = find_index tag in
    main ("\\label{section@the@hevea@index@" ^ tag ^ "}");
    main ("\\@indexsection{" ^ idx.name ^ "}") ;
    main "\\begin{the@hevea@index}";
    let indname = index_filename idx.sufout in
    begin match idx.from_file with
    | None ->
        create_hind  (Table.trim idx.from_doc) tag idx.sufout
    | _ -> ()
    end ;
    main ("\\input{"^indname^"}");
    main "\\end{the@hevea@index}"
  with
  | Not_found -> missing_index tag

let diff_entries e1 e2 =
  let l1 = Array.length e1 and l2 = Array.length e2 in
  if l1 <> l2 then true
  else
    let rec diff_rec i =
      if i >= l1 then false
      else
        e1.(i) <> e2.(i) || diff_rec (i+1) in
    diff_rec 0

let do_dump_index idxname idx = 
  try
    let chan = open_out idxname in
    Out.to_chan chan idx.out ;
    close_out chan
  with
  | Sys_error s ->
      Misc.warning
        ("File error on "^idxname^": "^s)          

let finalize check =
  if check then begin
    let top_changed = ref false in
    Hashtbl.iter
      (fun _ idx ->
        let entries = Table.trim idx.from_doc in
        let changed =
          match idx.from_file with
          | Some t -> diff_entries t entries
          | None   -> Array.length entries <> 0 in
        let idxname = index_filename idx.sufin in
        if changed || idx.onebad then begin
          top_changed := !top_changed || changed ;
          try
            if Array.length entries = 0 && not idx.onebad then
              Mysys.remove idxname 
            else begin
              do_dump_index idxname idx
            end
          with
          | Sys_error s ->
              Misc.warning
                ("File error on "^idxname^": "^s)
        end else if !Misc.dump_index then begin
          do_dump_index idxname idx
        end)
      itable ;
    if !top_changed then
      Misc.message
        "HeVeA Warning: Index(es) may have changed. Rerun me to get them right." ;
    !top_changed
  end else false

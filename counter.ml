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

let header = "$Id: counter.ml,v 1.9 1999-11-05 19:01:47 maranget Exp $" 
type t_counter =
    {mutable count : int ;
    mutable within : t_counter option ;
    mutable related : t_counter list}

let mk_bidon () = {count = 0 ; within = None ; related = []}

type t_checked =
    {cname : string ;
    cvalue : int ;
    cwithin : int option ;
    crelated : int list} 

let cbidon = {cname = "" ; cvalue = (-1) ; cwithin = None ; crelated = []}

let ctable = (Hashtbl.create 19 : (string,t_counter) Hashtbl.t);;

let check_ctable = ref [||]

let prerr_cc cc =
  prerr_endline ("counter: "^cc.cname) ;
  prerr_endline ("\tvalue = "^string_of_int cc.cvalue) ;
  prerr_endline
    ("\twithin = "^
     begin match cc.cwithin with
     | None -> "None"
     | Some j -> (!check_ctable).(j).cname
     end) ;
  prerr_string "\trelated =" ;
  List.iter
    (fun j ->
      prerr_string " " ;
      prerr_string (!check_ctable).(j).cname)
    cc.crelated ;
  prerr_endline ""

let checkpoint () =
  let module H = struct
    type t = t_counter 
    let equal = (==) 
    let hash = Hashtbl.hash
  end in
  let module RevHash = Hashtbl.Make (H) in
  let rev_table = RevHash.create 19
  and count = ref 0 in
  Hashtbl.iter
    (fun key value ->
      RevHash.add rev_table value (key, !count) ;
      incr count)
    ctable ;
  let to_int c =
    try
      let _,j = RevHash.find rev_table c in
      j
    with
    | Not_found -> Misc.fatal "Counter.checkpoint" in

  let t = Array.create !count cbidon in

  RevHash.iter
    (fun {count = value ; within = within ; related = related} (name, i) ->
      t.(i) <-
         {cname = name ;
         cvalue = value ;
         cwithin =
           begin match within with
           | None -> None
           | Some c -> Some (to_int c)
           end ;
        crelated = List.map to_int related})
    rev_table ;
  check_ctable := t

and hot_start () =
  
  Hashtbl.clear ctable ;
  let rec create_rec i =
    let cc = (!check_ctable).(i) in
    try
      Hashtbl.find ctable cc.cname
    with
    | Not_found ->
        let c =
          {count = cc.cvalue ; within = None ; related = []} in
        Hashtbl.add ctable cc.cname c;
        c.within <- begin match cc.cwithin with
          | None -> None
          | Some j -> Some (create_rec j) end ;
        c.related <- List.map create_rec cc.crelated ;
        if !Misc.verbose > 1 then begin 
          prerr_string "Restored " ;
          prerr_cc cc
        end ;
        c in
  for i = 0 to Array.length !check_ctable - 1 do
    let _ = create_rec i in ()
  done
;;

let unkown name where =
  Misc.warning ("Unknown counter: "^name^" in "^where)

let find_counter name = Hashtbl.find ctable name


let value_counter name =
  try
    let {count=c} = find_counter name in
    c
  with Not_found -> begin
    unkown name "\\value" ; 0
  end
;;

let def_counter name within =
  try
    let _ = Hashtbl.find ctable name in
    Misc.warning ("Counter "^name^" is already defined, not defining it") ;
    raise Latexmacros.Failed
  with
  | Not_found -> begin
      let within_c =
        try match within with "" -> None | _ -> Some (find_counter within)
        with Not_found -> begin
          unkown within ("\\newcounter{"^name^"}["^within^"]") ;
          None end in
      let c = {count=0 ; within=within_c ; related = []} in
      Hashtbl.add ctable name c ;
      match within_c with
      | Some d -> d.related <- c :: d.related
      | _ -> ()
  end

let number_within name within =
  try
    let c = find_counter name in
    begin match c.within with
    | Some d ->
        d.related <-
           List.fold_right (fun e r -> if e == c then r else e :: r)
             d.related []
    | _ -> ()
    end ;
    let d = find_counter within in
    c.within <- Some d ;
    d.related <- c :: d.related
  with Not_found ->
    unkown (name^" or "^within)  ("\\numberwithin")
  
let add_counter name i =
  try
   let c = find_counter name in
   c.count <- c.count + i
  with Not_found -> unkown name "\\addtocounter"
    
let set_counter name x =
  try
    let c = find_counter name in
    c.count <- x
  with Not_found -> unkown name "\\setcounter"
;;

let step_counter name =
  try
  let c = find_counter name in
  c.count <- c.count + 1;
  List.iter (fun c -> c.count <- 0) c.related
  with Not_found ->
    unkown name ("\\stepcounter")
;;




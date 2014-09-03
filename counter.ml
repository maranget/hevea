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

type t_counter =
    {mutable count : int ;
    mutable related : t_counter list}

type t_checked =
    {cname : string ;
    cvalue : int ;
    crelated : int list} 

let cbidon = {cname = "" ; cvalue = (-1) ; crelated = []}

let ctable = (Hashtbl.create 19 : (string,t_counter) Hashtbl.t);;

type saved =  t_checked array


let prerr_cc check_ctable cc =
  prerr_endline ("counter: "^cc.cname) ;
  prerr_endline ("\tvalue = "^string_of_int cc.cvalue) ;
  prerr_string "\trelated =" ;
  List.iter
    (fun j ->
      prerr_string " " ;
      prerr_string (check_ctable).(j).cname)
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

  let t = Array.make !count cbidon in

  RevHash.iter
    (fun {count = value ; related = related} (name, i) ->
      t.(i) <-
         {cname = name ;
         cvalue = value ;
        crelated = List.map to_int related})
    rev_table ;
  t

and hot_start check_ctable =
  
  Hashtbl.clear ctable ;
  let rec create_rec i =
    let cc = (check_ctable).(i) in
    try
      Hashtbl.find ctable cc.cname
    with
    | Not_found ->
        let c =
          {count = cc.cvalue ; related = []} in
        Hashtbl.add ctable cc.cname c;
        c.related <- List.map create_rec cc.crelated ;
        if !Misc.verbose > 1 then begin 
          prerr_string "Restored " ;
          prerr_cc check_ctable cc
        end ;
        c in
  for i = 0 to Array.length check_ctable - 1 do
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
      let c = {count=0 ; related = []} in
      Hashtbl.add ctable name c ;
      match within_c with
      | Some d -> d.related <- c :: d.related
      | _ -> ()
  end

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

let addtoreset name within =
  try
    let c = find_counter name in
    let d = find_counter within in
    d.related <- c :: d.related
  with Not_found ->
    unkown (name^" or "^within) "\\@addtoreset"
  
and removefromreset name within =
  try
    let c = find_counter name in
    let d = find_counter within in
    d.related <-
       List.fold_right
	 (fun e r -> if e == c then r else  e::r)
	 d.related []
  with Not_found ->
    unkown (name^" or "^within) "\\@removefromreset"


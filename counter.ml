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

let header = "$Id: counter.ml,v 1.5 1999-04-09 13:38:32 maranget Exp $" 
type t =
    {mutable count : int ;
    mutable within : t option ;
    mutable related : t list}
;;

let ctable = (Hashtbl.create 19 : (string,t) Hashtbl.t);;

let unkown name where =
  Parse_opts.warning ("Unknown counter: "^name^" in "^where)

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
   c.count <- c.count + 1
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

let refvalue = ref ""
;;

let setrefvalue  s = refvalue := s
and getrefvalue () = !refvalue
;;





type t = int ref * ((int ref)) list ref
;;

let ctable = (Hashtbl.create 19 : (string,t) Hashtbl.t);;


let find_counter name =
  try
    Hashtbl.find ctable name
  with Not_found -> begin
    Location.print_pos () ;
    prerr_string "Unknown counter: "; prerr_endline name ;
    ref 0,ref []
  end
;;

let value_counter name =
  let c,_ = find_counter name in
  !c
;;

let def_counter name within =
  let c =  ref 0 in
  Hashtbl.add ctable name (c,ref []) ;
  match within with
   "" -> ()
  | _ ->
     let _,l = find_counter within in
     l := c:: !l
;;

let add_counter name i =
  let c,_ = find_counter name in
  c := !c + i
;;
    
let set_counter name x =
  let c,_ = find_counter name in
  c := x
;;

let step_counter name =
  let c,l = find_counter name in
  c := !c + 1;
  List.iter (fun c -> c := 0) !l
;;

let refvalue = ref ""
;;

let setrefvalue  s = refvalue := s
and getrefvalue () = !refvalue
;;





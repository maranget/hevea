let ctable = (Hashtbl.create 19 : (string, int ref) Hashtbl.t);;

let def_counter name =
  Hashtbl.add ctable name (ref 0);;

let find_counter name =
  try
    Hashtbl.find ctable name
  with Not_found ->
    prerr_string "Unknown counter: "; prerr_endline name; (ref 0);;

let set_counter name x =
  let c = find_counter name in
  c := x
;;

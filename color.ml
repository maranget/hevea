let table = Hashtbl.create 17
;;

let to_hex x =
  Printf.sprintf "%02x" (truncate (255.0 *. x))
;;
exception Failed
;;

let define clr mdl value =
try
  let htmlval = match mdl with
    "gray" ->
      let x = Colscan.one (Lexing.from_string value) in
      let xx = to_hex x in
      xx^xx^xx
 | "rgb" ->
     let r,g,b =  Colscan.three(Lexing.from_string value) in
     to_hex r^to_hex g^to_hex b
 | _     ->
     Parse_opts.warning ("Color.define, unknown color model "^mdl);
     raise Failed in
  Hashtbl.add table clr htmlval
with Failed -> ()
;;

let retrieve clr =
  try
    Hashtbl.find table clr
  with Not_found ->
    Parse_opts.warning ("Colors.retrieve, unknown color :"^clr);
    raise Failed
;;


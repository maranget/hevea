let table = Hashtbl.create 17
;;

let to_hex x =
  Printf.sprintf "%02x" (truncate (255.0 *. x))
;;

let cmyk_to_rgb c m y k = 
  1.0 -. min 1.0 (c *. (1.0 -. k) +. k),
  1.0 -. min 1.0 (m *. (1.0 -. k) +. k),
  1.0 -. min 1.0 (y *. (1.0 -. k) +. k)
;;

exception Failed
;;

let define clr mdl value =
try
  let htmlval = match mdl with
    "gray" ->
      let x = Colscan.one (Lexing.from_string value) in
      let xx = to_hex x in
      "#"^xx^xx^xx
 | "rgb" ->
     let r,g,b =  Colscan.three(Lexing.from_string value) in
     "#"^to_hex r^to_hex g^to_hex b
 | "cmyk" ->
     let c,m,y,k = Colscan.four (Lexing.from_string value) in
     let r,g,b = cmyk_to_rgb c m y k in
     "#"^to_hex r^to_hex g^to_hex b
 | _     ->
     Parse_opts.warning ("Color.define, unknown color model "^mdl);
     raise Failed in
  Hashtbl.add table clr htmlval
with Failed -> ()
;;

define "blue" "rgb" "0, 0, 1" ;
define "red" "rgb"  "1, 0, 0" ;
define "green" "rgb" "0, 1, 0" ;
define "white" "gray" "1" ;
define "black" "gray" "0" ;
define "cyan" "cmyk" "1, 0, 0 , 0" ;
define "magenta" "cmyk" "0, 1, 0, 0" ;
define "yellow" "cmyk" "0, 0, 1, 0" ;
()
;;

let retrieve clr =
  try
    Hashtbl.find table clr
  with Not_found ->
    Parse_opts.warning ("Colors.retrieve, unknown color :"^clr);
    raise Failed
;;


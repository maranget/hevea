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

let do_compute mdl value = match mdl with
| "gray" ->
    let x = Colscan.one (Lexing.from_string value) in
    let xx = to_hex x in
    xx^xx^xx
| "rgb" ->
    let r,g,b =  Colscan.three(Lexing.from_string value) in
    to_hex r^to_hex g^to_hex b
| "cmyk" ->
    let c,m,y,k = Colscan.four (Lexing.from_string value) in
    let r,g,b = cmyk_to_rgb c m y k in
    to_hex r^to_hex g^to_hex b
| "named" -> begin
    try Hashtbl.find table ("named@"^value) with
    | Not_found -> begin
        Misc.warning ("Unkown name in the named color model: "^value) ;
        raise Failed
    end
end
| _     ->
    Misc.warning ("Color.compute, unknown color model: "^mdl);
    raise Failed



let compute mdl value =
  try do_compute mdl value with Failed -> ""

let define clr mdl value =
try
  Hashtbl.add table clr (do_compute mdl value)
with Failed -> ()
;;

let retrieve clr =
  try
    Hashtbl.find table clr
  with Not_found ->
    Misc.warning ("Colors.retrieve, unknown color: "^clr);
    ""
;;


let define_named name mdl value = define ("named@"^name) mdl value
;;


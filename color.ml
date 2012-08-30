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

type t = Name of string | Hex of string

let default_color = Name "black"
;;

let table = (Hashtbl.create 17 : (string, t) Hashtbl.t)
;;

type saved = (string, t) Hashtbl.t

let checkpoint () = 
  let ctable = Hashtbl.create 17 in
  Misc.copy_hashtbl table ctable ;
  ctable

and hot_start ctable = Misc.copy_hashtbl ctable table

let to_hex x =
  Printf.sprintf "%02X" (truncate (255.0 *. x))
;;

let cmyk_to_rgb c m y k = 
  1.0 -. min 1.0 (c *. (1.0 -. k) +. k),
  1.0 -. min 1.0 (m *. (1.0 -. k) +. k),
  1.0 -. min 1.0 (y *. (1.0 -. k) +. k)
;;

let hls_to_rgb h l s =
  let rgb q1 q2 hue =
     let hue =
       if hue > 360.0 then  hue -. 360.0
       else if hue < 0.0 then hue +. 360.0
       else hue in
     if hue < 60.0 then
       q1 +. (q2 -. q1) /. 60.0
     else if hue < 180.0 then
       q2
     else if hue < 240.0 then
       q1 +. (q2 -. q1) *. (240.0 -. hue) /. 60.0
     else
       q1 in
  let p2 =
    if l <= 0.5 then l *. (1.0 +. s)
    else l +. s -. (l *. s) in
  let p1 = 2.0 *. l -. p2 in
  if s = 0.0 then
    l,l,l
  else
    rgb p1 p2 (h +. 100.0),
    rgb p1 p2 h,
    rgb p1 p2 (h -. 120.0)
;;
    
let hsv_to_rgb h s v =
  if s = 0.0 then v,v,v
  else
    let h = h /. 60.0 in
    let i = truncate h in
    let f = h -. float i in
    let p = v *. (1.0 -. s) in
    let q = v *. (1.0 -. (s *. f)) in
    let t = v *. (1.0 -. (s *. (1.0 -. f))) in
    match i with
    | 0 -> v,t,p
    | 1 -> q,v,p
    | 2 -> p,v,t
    | 3 -> p,q,v
    | 4 -> t,p,v
    | 5 -> v,p,q
    | _ -> Misc.fatal ("Bad HSV color specification")
;;

        

exception Failed
;;

let names = Hashtbl.create 17

let _ =
  List.iter
    (fun (xx,name) -> Hashtbl.add names xx name)
  [ "000000", "black" ;
  "C0C0C0", "silver" ;
  "808080", "gray" ;
  "FFFFFF", "white" ;
  "800000", "maroon" ;
  "FF0000", "red" ;
  "800080", "purple" ;
  "FF00FF", "fuchsia" ;
  "008000", "green" ;
  "00FF00", "lime" ;
  "808000", "olive" ;
  "FFFF00", "yellow" ;
  "000080", "navy" ;
  "0000FF", "blue" ;
  "008080", "teal" ;
  "00FFFF", "aqua" ;
  ] 

let do_compute mdl value =
  match mdl with
  | "named" ->
      begin
        try Hashtbl.find table ("named@"^value) with
        | Not_found -> begin
            Misc.warning ("Unkown name in the named color model: "^value) ;
            raise Failed
        end
      end
  | _ ->
      let res = match mdl with
      | "gray" ->
          let x = Colscan.one (MyLexing.from_string value) in
          let xx = to_hex x in
          xx^xx^xx
      | "rgb" ->
          let r,g,b =  Colscan.three(MyLexing.from_string value) in
          to_hex r^to_hex g^to_hex b
      | "cmyk" ->
          let c,m,y,k = Colscan.four (MyLexing.from_string value) in
          let r,g,b = cmyk_to_rgb c m y k in
          to_hex r^to_hex g^to_hex b
      | "hsv" ->
          let h,s,v = Colscan.three (MyLexing.from_string value) in
          let r,g,b = hsv_to_rgb h s v in
          to_hex r^to_hex g^to_hex b
      | "hls" ->
          let h,l,s = Colscan.three (MyLexing.from_string value) in
          let r,g,b = hls_to_rgb h l s in
          to_hex r^to_hex g^to_hex b
      | _     ->
          Misc.warning ("Color.compute, unknown color model: "^mdl);
          raise Failed in
      try
        Name (Hashtbl.find names res)
      with Not_found -> Hex res


let compute mdl value =
  try do_compute mdl value with Failed -> default_color

let define clr mdl value =
try
  Hashtbl.add table clr (do_compute mdl value)
with Failed -> ()
;;

let retrieve clr =
  try
    Hashtbl.find table clr
  with Not_found ->
    Misc.warning ("Color.retrieve, unknown color: "^clr);
    default_color
;;


let define_named name mdl value = define ("named@"^name) mdl value
;;

let remove clr = Hashtbl.remove table clr

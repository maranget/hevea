(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet MOSCOVA, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2006 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

exception Failed of string

let bad_char c =
  raise
    (Failed
       (Printf.sprintf "Bad character in unicode entity: %c" c))

let rec parse10 len str i r =
  if i < len then
    let c = match str.[i] with
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4
    | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9
    | c -> bad_char c in
    parse10 len str (i+1) (10*r+c)
  else
    r

let rec parse16 len str i r =
  if i < len then
    let c = match str.[i] with
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4
    | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9
    | 'a'|'A' -> 10 | 'b'|'B' -> 11 | 'c'|'C' -> 12
    | 'd'|'D' -> 13 | 'e'|'E' -> 14 | 'f'|'F' -> 15
    | c -> bad_char c in
    parse16 len str (i+1) (16*r+c)
  else
    r

let do_parse str =
  let len = String.length str in
  if len = 0 then begin 
    raise (Failed "Cannot parse unicode entity: empty")
  end else
    match str.[0] with
    | 'X'|'x' -> parse16 len str 1 0
    | '0'..'9' -> parse10 len str 0 0
    | c -> bad_char c
	
let parse str =
  try do_parse str
  with Failed msg ->
    Misc.warning msg ;
    0

exception CannotTranslate

let translate_ascii_out i =
  if i < 128 then Char.chr i
  else raise CannotTranslate

and translate_ascii_in c =
  let i = Char.code c in
  if i < 128 then i
  else raise CannotTranslate    

let translate_latin1_out i =
  if i < 256 then Char.chr i
  else raise CannotTranslate

and translate_latin1_in c = Char.code c

let translate_out_fun = ref translate_ascii_out
and translate_in_fun = ref translate_ascii_in

let make_out_translator ps =
  let t = Hashtbl.create 101 in
  List.iter (fun (iso, uni) -> Hashtbl.add t uni (Char.chr iso)) ps ;
  (fun i ->
    try Hashtbl.find t i
    with Not_found -> raise CannotTranslate)

and make_in_translator ps =
  let t = Array.create 256 0 in
  List.iter (fun (iso, uni) -> t.(iso) <- uni) ps ;
  (fun c -> t.(Char.code c))

let read_mapping name chan =
  let t = ref []
  and scanbuf = Scanf.Scanning.from_channel chan in
  try
    while true do
      Scanf.bscanf scanbuf " %i %i"
        (fun iso uni -> t := (iso,uni) :: !t) ;
    done ;
    !t
  with
  | End_of_file -> !t
  | Sys_error msg ->
      Misc.warning
        (Printf.sprintf
           "Error '%s' while loading mapping: %s\n"
           msg name) ;
      raise (Misc.Fatal "Mapping")
 
let open_mapping name =
  try
    let real_name = Myfiles.find name in
     open_in real_name
  with
  | Not_found ->
      Misc.warning
        (Printf.sprintf "Cannot find mapping: %s\n" name) ;
      raise (Misc.Fatal "Mapping")
  | Sys_error msg ->
      Misc.warning
        (Printf.sprintf
           "Error '%s' while opening mapping: %s\n"
           msg name) ;
      raise (Misc.Fatal "Mapping")

and close_mapping chan = try close_in chan with _ -> ()


let set_output_translator name =
  let key = Filename.basename name in
  match key with
  | "ISO-8859-1.map" ->
      translate_out_fun := translate_latin1_out
  | "US-ASCII.map" ->
      translate_out_fun := translate_ascii_out
  | _ ->
      let chan = open_mapping name in
      let ps = read_mapping name chan in
      close_mapping chan ;
      translate_out_fun := make_out_translator ps

and set_input_translator name =
  let key = Filename.basename name in
  match key with
  | "ISO-8859-1.map" ->
      translate_in_fun := translate_latin1_in
  | "US-ASCII.map" ->
      translate_in_fun := translate_ascii_in
  | _ ->
      let chan = open_mapping name in
      let ps = read_mapping name chan in
      close_mapping chan ;
      translate_in_fun := make_in_translator ps

and set_translators name =
  let key = Filename.basename name in
  match key with
  | "ISO-8859-1.map" ->
      translate_out_fun := translate_latin1_out ;
      translate_in_fun := translate_latin1_in
  | "US-ASCII.map" ->
      translate_out_fun := translate_ascii_out ;
      translate_in_fun := translate_ascii_in
  | _ ->
      let chan = open_mapping name in
      let ps = read_mapping name chan in
      close_mapping chan ;
      translate_out_fun := make_out_translator ps ;
      translate_in_fun := make_in_translator ps
  
  
let translate_out i = !translate_out_fun i
and translate_in c = !translate_in_fun c

(* Diacritical marks *)

(*
  Tables from ftp://ftp.unicode.org/Public/MAPPINGS/ISO8859
  Mapping from NAME LIST of the www.unicode.org site.
    
  Got functions by:
    egrep 'WITH GRAVE$' NamesList.txt | grep LATIN | awk -f a.awk

Where a.awk is
/SMALL/ { printf ("| '%s' -> 0x%s\n", tolower($5), $1) }
/CAPITAL/ { printf ("| '%s' -> 0x%s\n", $5, $1) }

*)

let grave = function
  | 'A' -> 0x00C0
  | 'E' -> 0x00C8
  | 'I' -> 0x00CC
  | 'O' -> 0x00D2
  | 'U' -> 0x00D9
  | 'a' -> 0x00E0
  | 'e' -> 0x00E8
  | 'i' -> 0x00EC
  | 'o' -> 0x00F2
  | 'u' -> 0x00F9
  | 'N' -> 0x01F8
  | 'n' -> 0x01F9
  | 'W' -> 0x1E80
  | 'w' -> 0x1E81
  | 'Y' -> 0x1EF2
  | 'y' -> 0x1EF3
  | _ -> raise CannotTranslate

and acute = function
  | 'A' -> 0x00C1
  | 'E' -> 0x00C9
  | 'I' -> 0x00CD
  | 'O' -> 0x00D3
  | 'U' -> 0x00DA
  | 'Y' -> 0x00DD
  | 'a' -> 0x00E1
  | 'e' -> 0x00E9
  | 'i' -> 0x00ED
  | 'o' -> 0x00F3
  | 'u' -> 0x00FA
  | 'y' -> 0x00FD
  | 'C' -> 0x0106
  | 'c' -> 0x0107
  | 'L' -> 0x0139
  | 'l' -> 0x013A
  | 'N' -> 0x0143
  | 'n' -> 0x0144
  | 'R' -> 0x0154
  | 'r' -> 0x0155
  | 'S' -> 0x015A
  | 's' -> 0x015B
  | 'Z' -> 0x0179
  | 'z' -> 0x017A
  | 'G' -> 0x01F4
  | 'g' -> 0x01F5
  | 'K' -> 0x1E30
  | 'k' -> 0x1E31
  | 'M' -> 0x1E3E
  | 'm' -> 0x1E3F
  | 'P' -> 0x1E54
  | 'p' -> 0x1E55
  | 'W' -> 0x1E82
  | 'w' -> 0x1E83
  | _ -> raise CannotTranslate

and circumflex = function
  | 'A' -> 0x00C2
  | 'E' -> 0x00CA
  | 'I' -> 0x00CE
  | 'O' -> 0x00D4
  | 'U' -> 0x00DB
  | 'a' -> 0x00E2
  | 'e' -> 0x00EA
  | 'i' -> 0x00EE
  | 'o' -> 0x00F4
  | 'u' -> 0x00FB
  | 'C' -> 0x0108
  | 'c' -> 0x0109
  | 'G' -> 0x011C
  | 'g' -> 0x011D
  | 'H' -> 0x0124
  | 'h' -> 0x0125
  | 'J' -> 0x0134
  | 'j' -> 0x0135
  | 'S' -> 0x015C
  | 's' -> 0x015D
  | 'W' -> 0x0174
  | 'w' -> 0x0175
  | 'Y' -> 0x0176
  | 'y' -> 0x0177
  | 'Z' -> 0x1E90
  | 'z' -> 0x1E91
  | _ -> raise CannotTranslate

and tilde = function
  | 'A' -> 0x00C3
  | 'N' -> 0x00D1
  | 'O' -> 0x00D5
  | 'a' -> 0x00E3
  | 'n' -> 0x00F1
  | 'o' -> 0x00F5
  | 'I' -> 0x0128
  | 'i' -> 0x0129
  | 'U' -> 0x0168
  | 'u' -> 0x0169
  | 'V' -> 0x1E7C
  | 'v' -> 0x1E7D
  | 'E' -> 0x1EBC
  | 'e' -> 0x1EBD
  | 'Y' -> 0x1EF8
  | 'y' -> 0x1EF9
  | _ -> raise CannotTranslate

and diaeresis = function
  | 'A' -> 0x00C4
  | 'E' -> 0x00CB
  | 'I' -> 0x00CF
  | 'O' -> 0x00D6
  | 'U' -> 0x00DC
  | 'a' -> 0x00E4
  | 'e' -> 0x00EB
  | 'i' -> 0x00EF
  | 'o' -> 0x00F6
  | 'u' -> 0x00FC
  | 'y' -> 0x00FF
  | 'Y' -> 0x0178
  | 'H' -> 0x1E26
  | 'h' -> 0x1E27
  | 'W' -> 0x1E84
  | 'w' -> 0x1E85
  | 'X' -> 0x1E8C
  | 'x' -> 0x1E8D
  | 't' -> 0x1E97
  | _ -> raise CannotTranslate

and ring = function
  | 'A' -> 0x00C5
  | 'a' -> 0x00E5
  | 'U' -> 0x016E
  | 'u' -> 0x016F
  | 'w' -> 0x1E98
  | 'y' -> 0x1E99
  | _ -> raise CannotTranslate

and cedilla = function
  | 'C' -> 0x00C7
  | 'c' -> 0x00E7
  | 'G' -> 0x0122
  | 'g' -> 0x0123
  | 'K' -> 0x0136
  | 'k' -> 0x0137
  | 'L' -> 0x013B
  | 'l' -> 0x013C
  | 'N' -> 0x0145
  | 'n' -> 0x0146
  | 'R' -> 0x0156
  | 'r' -> 0x0157
  | 'S' -> 0x015E
  | 's' -> 0x015F
  | 'T' -> 0x0162
  | 't' -> 0x0163
  | 'E' -> 0x0228
  | 'e' -> 0x0229
  | 'D' -> 0x1E10
  | 'd' -> 0x1E11
  | 'H' -> 0x1E28
  | 'h' -> 0x1E29
  | _ -> raise CannotTranslate

and stroke = function
  | 'O' -> 0x00D8
  | 'o' -> 0x00F8
  | 'D' -> 0x0110
  | 'd' -> 0x0111
  | 'H' -> 0x0126
  | 'h' -> 0x0127
  | 'L' -> 0x0141
  | 'l' -> 0x0142
  | 'T' -> 0x0166
  | 't' -> 0x0167
  | 'b' -> 0x0180
  | 'I' -> 0x0197
  | 'Z' -> 0x01B5
  | 'z' -> 0x01B6
  | 'G' -> 0x01E4
  | 'g' -> 0x01E5
  | 'A' -> 0x023A
  | 'C' -> 0x023B
  | 'c' -> 0x023C
  | 'i' -> 0x0268
  | 'p' -> 0x1D7D
  | _ -> raise CannotTranslate

and macron = function
  | 'A' -> 0x0100
  | 'a' -> 0x0101
  | 'E' -> 0x0112
  | 'e' -> 0x0113
  | 'I' -> 0x012A
  | 'i' -> 0x012B
  | 'O' -> 0x014C
  | 'o' -> 0x014D
  | 'U' -> 0x016A
  | 'u' -> 0x016B
(* Aie
   | 'AE' -> 0x01E2
   | 'ae' -> 0x01E3
   *)
  | 'Y' -> 0x0232
  | 'y' -> 0x0233
  | 'G' -> 0x1E20
  | 'g' -> 0x1E21
  | _ -> raise CannotTranslate

and caron = function
  | 'C' -> 0x010C
  | 'c' -> 0x010D
  | 'D' -> 0x010E
  | 'd' -> 0x010F
  | 'E' -> 0x011A
  | 'e' -> 0x011B
  | 'L' -> 0x013D
  | 'l' -> 0x013E
  | 'N' -> 0x0147
  | 'n' -> 0x0148
  | 'R' -> 0x0158
  | 'r' -> 0x0159
  | 'S' -> 0x0160
  | 's' -> 0x0161
  | 'T' -> 0x0164
  | 't' -> 0x0165
  | 'Z' -> 0x017D
  | 'z' -> 0x017E
(*
  | 'DZ' -> 0x01C4
  | 'dz' -> 0x01C6
  *)
  | 'A' -> 0x01CD
  | 'a' -> 0x01CE
  | 'I' -> 0x01CF
  | 'i' -> 0x01D0
  | 'O' -> 0x01D1
  | 'o' -> 0x01D2
  | 'U' -> 0x01D3
  | 'u' -> 0x01D4
  | 'G' -> 0x01E6
  | 'g' -> 0x01E7
  | 'K' -> 0x01E8
  | 'k' -> 0x01E9
(*Aie
  | 'EZH' -> 0x01EE
  | 'ezh' -> 0x01EF
  *)
  | 'j' -> 0x01F0
  | 'H' -> 0x021E
  | 'h' -> 0x021F
  | _ -> raise CannotTranslate

and doubleacute = function
  | 'O' -> 0x0150
  | 'o' -> 0x0151
  | 'U' -> 0x0170
  | 'u' -> 0x0171
  | _ -> raise CannotTranslate

and doublegrave = function
  | 'A' -> 0x0200
  | 'a' -> 0x0201
  | 'E' -> 0x0204
  | 'e' -> 0x0205
  | 'I' -> 0x0208
  | 'i' -> 0x0209
  | 'O' -> 0x020C
  | 'o' -> 0x020D
  | 'R' -> 0x0210
  | 'r' -> 0x0211
  | 'U' -> 0x0214
  | 'u' -> 0x0215
  | _ -> raise CannotTranslate

and breve = function
  | 'A' -> 0x0102
  | 'a' -> 0x0103
  | 'E' -> 0x0114
  | 'e' -> 0x0115
  | 'G' -> 0x011E
  | 'g' -> 0x011F
  | 'I' -> 0x012C
  | 'i' -> 0x012D
  | 'O' -> 0x014E
  | 'o' -> 0x014F
  | 'U' -> 0x016C
  | 'u' -> 0x016D
  | 'H' -> 0x1E2A
  | 'h' -> 0x1E2B
  | _ -> raise CannotTranslate

and dotabove = function
  | 'C' -> 0x010A
  | 'c' -> 0x010B
  | 'E' -> 0x0116
  | 'e' -> 0x0117
  | 'G' -> 0x0120
  | 'g' -> 0x0121
  | 'I' -> 0x0130
  | 'i' -> 0x69
  | 'Z' -> 0x017B
  | 'z' -> 0x017C
  | 'A' -> 0x0226
  | 'a' -> 0x0227
  | 'O' -> 0x022E
  | 'o' -> 0x022F
  | 'B' -> 0x1E02
  | 'b' -> 0x1E03
  | 'D' -> 0x1E0A
  | 'd' -> 0x1E0B
  | 'F' -> 0x1E1E
  | 'f' -> 0x1E1F
  | 'H' -> 0x1E22
  | 'h' -> 0x1E23
  | 'M' -> 0x1E40
  | 'm' -> 0x1E41
  | 'N' -> 0x1E44
  | 'n' -> 0x1E45
  | 'P' -> 0x1E56
  | 'p' -> 0x1E57
  | 'R' -> 0x1E58
  | 'r' -> 0x1E59
  | 'S' -> 0x1E60
  | 's' -> 0x1E61
  | 'T' -> 0x1E6A
  | 't' -> 0x1E6B
  | 'W' -> 0x1E86
  | 'w' -> 0x1E87
  | 'X' -> 0x1E8A
  | 'x' -> 0x1E8B
  | 'Y' -> 0x1E8E
  | 'y' -> 0x1E8F
  | _ -> raise CannotTranslate

and dotbelow = function
  | 'B' -> 0x1E04
  | 'b' -> 0x1E05
  | 'D' -> 0x1E0C
  | 'd' -> 0x1E0D
  | 'H' -> 0x1E24
  | 'h' -> 0x1E25
  | 'K' -> 0x1E32
  | 'k' -> 0x1E33
  | 'L' -> 0x1E36
  | 'l' -> 0x1E37
  | 'M' -> 0x1E42
  | 'm' -> 0x1E43
  | 'N' -> 0x1E46
  | 'n' -> 0x1E47
  | 'R' -> 0x1E5A
  | 'r' -> 0x1E5B
  | 'S' -> 0x1E62
  | 's' -> 0x1E63
  | 'T' -> 0x1E6C
  | 't' -> 0x1E6D
  | 'V' -> 0x1E7E
  | 'v' -> 0x1E7F
  | 'W' -> 0x1E88
  | 'w' -> 0x1E89
  | 'Z' -> 0x1E92
  | 'z' -> 0x1E93
  | 'A' -> 0x1EA0
  | 'a' -> 0x1EA1
  | 'E' -> 0x1EB8
  | 'e' -> 0x1EB9
  | 'I' -> 0x1ECA
  | 'i' -> 0x1ECB
  | 'O' -> 0x1ECC
  | 'o' -> 0x1ECD
  | 'U' -> 0x1EE4
  | 'u' -> 0x1EE5
  | 'Y' -> 0x1EF4
  | 'y' -> 0x1EF5
  | _ -> raise CannotTranslate

and linebelow = function
  | 'B' -> 0x1E06
  | 'b' -> 0x1E07
  | 'D' -> 0x1E0E
  | 'd' -> 0x1E0F
  | 'K' -> 0x1E34
  | 'k' -> 0x1E35
  | 'L' -> 0x1E3A
  | 'l' -> 0x1E3B
  | 'N' -> 0x1E48
  | 'n' -> 0x1E49
  | 'R' -> 0x1E5E
  | 'r' -> 0x1E5F
  | 'T' -> 0x1E6E
  | 't' -> 0x1E6F
  | 'Z' -> 0x1E94
  | 'z' -> 0x1E95
  | 'h' -> 0x1E96
  | _ -> raise CannotTranslate

and ringabove = function
  | 'A' -> 0x00C5
  | 'a' -> 0x00E5
  | 'U' -> 0x016E
  | 'u' -> 0x016F
  | 'w' -> 0x1E98
  | 'y' -> 0x1E99
  | _ -> raise CannotTranslate

and ogonek = function
  | 'A' -> 0x0104
  | 'a' -> 0x0105
  | 'E' -> 0x0118
  | 'e' -> 0x0119
  | 'I' -> 0x012E
  | 'i' -> 0x012F
  | 'U' -> 0x0172
  | 'u' -> 0x0173
  | 'O' -> 0x01EA
  | 'o' -> 0x01EB
  | _ -> raise CannotTranslate

and circled = function
  | 'A' -> 0x24B6
  | 'B' -> 0x24B7
  | 'C' -> 0x24B8
  | 'D' -> 0x24B9
  | 'E' -> 0x24BA
  | 'F' -> 0x24BB
  | 'G' -> 0x24BC
  | 'H' -> 0x24BD
  | 'I' -> 0x24BE
  | 'J' -> 0x24BF
  | 'K' -> 0x24C0
  | 'L' -> 0x24C1
  | 'M' -> 0x24C2
  | 'N' -> 0x24C3
  | 'O' -> 0x24C4
  | 'P' -> 0x24C5
  | 'Q' -> 0x24C6
  | 'R' -> 0x24C7
  | 'S' -> 0x24C8
  | 'T' -> 0x24C9
  | 'U' -> 0x24CA
  | 'V' -> 0x24CB
  | 'W' -> 0x24CC
  | 'X' -> 0x24CD
  | 'Y' -> 0x24CE
  | 'Z' -> 0x24CF
  | 'a' -> 0x24D0
  | 'b' -> 0x24D1
  | 'c' -> 0x24D2
  | 'd' -> 0x24D3
  | 'e' -> 0x24D4
  | 'f' -> 0x24D5
  | 'g' -> 0x24D6
  | 'h' -> 0x24D7
  | 'i' -> 0x24D8
  | 'j' -> 0x24D9
  | 'k' -> 0x24DA
  | 'l' -> 0x24DB
  | 'm' -> 0x24DC
  | 'n' -> 0x24DD
  | 'o' -> 0x24DE
  | 'p' -> 0x24DF
  | 'q' -> 0x24E0
  | 'r' -> 0x24E1
  | 's' -> 0x24E2
  | 't' -> 0x24E3
  | 'u' -> 0x24E4
  | 'v' -> 0x24E5
  | 'w' -> 0x24E6
  | 'x' -> 0x24E7
  | 'y' -> 0x24E8
  | 'z' -> 0x24E9
  | _ -> raise CannotTranslate

and doublestruck = function
  | 'C' -> 0x2102
  | 'H' -> 0x210D
  | 'N' -> 0x2115
  | 'P' -> 0x2119
  | 'Q' -> 0x211A
  | 'R' -> 0x211D
  | 'Z' -> 0x2124
  | 'D' -> 0x2145
  | 'd' -> 0x2146
  | 'e' -> 0x2147
  | 'i' -> 0x2148
  | 'j' -> 0x2149
  |  c ->
      if !Parse_opts.moreentities then begin match c with
      | 'A' -> 0x1D538
      | 'B' -> 0x1D539
      | 'E' -> 0x1D53C
      | 'F' -> 0x1D53D
      | 'G' -> 0x1D53E
      | 'I' -> 0x1D540
      | 'J' -> 0x1D541
      | 'K' -> 0x1D542
      | 'L' -> 0x1D543
      | 'M' -> 0x1D544
      | 'O' -> 0x1D546
      | 'S' -> 0x1D54A
      | 'T' -> 0x1D54B
      | 'U' -> 0x1D54C
      | 'V' -> 0x1D54D
      | 'W' -> 0x1D54E
      | 'X' -> 0x1D54F
      | 'Y' -> 0x1D550
      | 'a' -> 0x1D552
      | 'b' -> 0x1D553
      | 'c' -> 0x1D554
      | 'f' -> 0x1D557
      | 'g' -> 0x1D558
      | 'h' -> 0x1D559
      | 'k' -> 0x1D55C
      | 'l' -> 0x1D55D
      | 'm' -> 0x1D55E
      | 'n' -> 0x1D55F
      | 'o' -> 0x1D560
      | 'p' -> 0x1D561
      | 'q' -> 0x1D562
      | 'r' -> 0x1D563
      | 's' -> 0x1D564
      | 't' -> 0x1D565
      | 'u' -> 0x1D566
      | 'v' -> 0x1D567
      | 'w' -> 0x1D568
      | 'x' -> 0x1D569
      | 'y' -> 0x1D56A
      | 'z' -> 0x1D56B
      | '0' -> 0x1D7D8
      | '1' -> 0x1D7D9
      | '2' -> 0x1D7DA
      | '3' -> 0x1D7DB
      | '4' -> 0x1D7DC
      | '5' -> 0x1D7DD
      | '6' -> 0x1D7DE
      | '7' -> 0x1D7DF
      | '8' -> 0x1D7E0
      | '9' -> 0x1D7E1
      | _ -> raise CannotTranslate 
      end else
	raise CannotTranslate
(* Text rendering *)
let def_t = Hashtbl.create 101

let def_default i default = Hashtbl.replace def_t i default
and get_default i = Hashtbl.find def_t i

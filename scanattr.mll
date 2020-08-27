(*
 *  This file is distributed under the terms of the GNU General Public
 *  License either version 2 of the License, or (at your option) any
 *  later version, as detailed in the file LICENSE.
 *
 *  Copyright (C) 2020  Christoph L. Spiel
 *)


{
  type html_attribute = {
    name: string;
    value: string;
  }
}


let optional_white = ['\t' '\n' '\r' ' ']*
let attribute_name = [^ '\x00'-'\x1F' '\x20' '\x22' '\x27' '\x2F' '\x3D' '\x3E' '\x7F'-'\x9F']+
let unquoted_value = [^ '\x20' '"' '\'' '=' '<' '>' '`']+
let single_quoted_value = [^ '\'']+
let double_quoted_value = [^ '"']+


rule scan attributes = parse
| ['\t' '\n' '\r' ' ']+
  {
    scan attributes lexbuf
  }
| (attribute_name as name)
  {
    {name; value = ""} :: attributes
  }
| (attribute_name as name) optional_white '=' optional_white (unquoted_value as attribute_value)
  {
    {name; value = attribute_value} :: attributes
  }
| (attribute_name as name) optional_white '=' optional_white '\'' (single_quoted_value as attribute_value) '\''
  {
    {name; value = attribute_value} :: attributes
  }
| (attribute_name as name) optional_white '=' optional_white '"' (double_quoted_value as attribute_value) '"'
  {
    {name; value = attribute_value} :: attributes
  }
| eof
  {
    []
  }


{
  let scan_html_attribute a_lexbuffer =
    let rec scan_all a =
      match scan a a_lexbuffer with
      | [] -> List.rev a
      | xs -> scan_all xs
    in
      scan_all []
}

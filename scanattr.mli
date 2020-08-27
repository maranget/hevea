(*
 *  This file is distributed under the terms of the GNU General Public
 *  License either version 2 of the License, or (at your option) any
 *  later version, as detailed in the file LICENSE.
 *
 *  Copyright (C) 2020  Christoph L. Spiel
 *)


type html_attribute = {
    name: string;
    value: string;
}


val scan_html_attribute: Lexing.lexbuf -> html_attribute list

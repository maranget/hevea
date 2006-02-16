{
open Printf
} 

rule line = parse
| ('#' [^'\n']* '\n' | '\n')
  {line lexbuf }
| [^'#''\n']+ as txt ('#' [^'\n']*) ? '\n'
  { printf "%s\n" txt ; line lexbuf }
| eof { () }

{
let main () = line (Lexing.from_channel stdin)

let _ = main () ; exit 0

} 


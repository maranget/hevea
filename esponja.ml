
let pess = ref false
;;

let arg = ref []
;;

Arg.parse
  ["-u", Arg.Set pess, "pessimize" ;
  "-v", Arg.Unit (fun () -> incr Ultra.verbose),"be verbose"]
  (fun s -> arg :=  s :: !arg)
  ("Usage: esponja [option*] < infile > outfile,\n or    esponja [option*] files+
options are:")
;;


let process in_name input output =
  let rec do_rec lexbuf = match Htmlparse.main lexbuf with
  | [] -> ()
  | ts ->
      if !pess then
        Pp.trees output (Explode.trees ts)
      else
        Pp.trees output (Ultra.main ts) ;
      do_rec lexbuf in
  try
    let lexbuf = Lexing.from_channel input in
    Location.set in_name lexbuf ;
    do_rec lexbuf ;
    Location.restore () ;
    true
  with
  | Htmllex.Error s ->
      Location.print_pos () ;
      Printf.fprintf stderr "Lexer error: %s\n" s ;
      false
  | Htmlparse.Error s ->
      Location.print_pos () ;
      Printf.fprintf stderr "Parser error: %s\n" s ;
      false
        
        
let main () =
  try
    begin match !arg with
    | [] ->
        ignore (process "" stdin stdout)
    | files ->
        List.iter
          (fun in_name ->
            let out_name = Filename.temp_file "esponja" ".html" in
            begin try
              let input = open_in in_name in
              let out =
                try open_out out_name
                with Sys_error _ as e ->
                  close_in input ; raise e in
              let ok = process in_name input out in
              close_in input ;
              close_out out ;
              if ok then
                Sys.rename out_name in_name
            with
            | Sys_error msg ->
                Printf.fprintf stderr "File error: %s\n" msg
            | e ->
                Sys.remove out_name ;
                raise e
            end)
          files
    end ;
    exit 0
  with
  | e ->
      Printf.fprintf stderr "Unexpected exception: %s\n"
        (Printexc.to_string e) ;
      exit 1
;;

main ()
;;

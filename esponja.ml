
let pess  = ref false
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


let copy from_name to_name =
  let size = 1024 in
  let buff = String.create size in
  let chan_in = open_in_bin from_name in
  let  chan_out =
    try open_out_bin to_name
    with Sys_error _ as e ->
      close_in chan_in ; raise e in
  let rec do_rec () =
    let i = input chan_in buff 0 size in
    if i > 0 then begin
      output chan_out buff 0 i ;
      do_rec ()
    end in
  begin try
    do_rec ()
  with
  | e ->
      close_in chan_in ;
      close_out chan_out ; raise e
  end ;
  close_in chan_in ;
  close_out chan_out
;;


let remove name =
  try
    Sys.remove name
  with
  | Sys_error _ -> ()

let length input  = in_channel_length input

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
      if !Ultra.verbose > 0 then
        output_char stderr '\n' ;
      Location.print_pos () ;
      Printf.fprintf stderr "Lexer error: %s\n" s ;
      false
  | Htmlparse.Error s ->
      if !Ultra.verbose > 0 then
        output_char stderr '\n' ;
      Location.print_pos () ;
      Printf.fprintf stderr "Parser error: %s\n" s ;
      false
        

let rename from_name to_name =
 if Sys.file_exists to_name then
    Sys.remove to_name ;
  (* So long for atomicity ! *)
  try
    Sys.rename from_name to_name 
  with
  | Sys_error _ -> (* Well we need to copy... *)
      try
        let dir = Filename.dirname to_name in
        let tmp_name = Filename.concat dir (".#"^Filename.basename to_name) in
        copy from_name tmp_name ;
        Sys.rename tmp_name to_name ;
        remove from_name
      with
      | Sys_error _ -> remove from_name


let main () =
  try
    begin match !arg with
    | [] ->
        ignore (process "" stdin stdout)
    | files ->
        List.iter
          (fun in_name ->
            if !Ultra.verbose > 0 then begin
              Printf.fprintf stderr "Processing file: %s... " in_name ;
              flush stderr
            end ;
            let out_name = Filename.temp_file "esponja" ".html" in
            begin try
              let input = open_in in_name in
              let out =
                try open_out out_name
                with Sys_error _ as e ->
                  close_in input ; raise e in
              let size_in = in_channel_length input in
              let ok = process in_name input out in
              flush out ;
              let size_out = out_channel_length out in
              close_in input ;
              close_out out ;
              if ok && size_in > size_out then
                rename out_name in_name
              else
                remove out_name ;
              if !Ultra.verbose > 0  && ok then begin
                Printf.fprintf stderr "saved %d -> %d %0.2f"
                  size_in size_out
                  ((float (size_in-size_out) *. 100.0) /.
                   float size_in) ;
                  prerr_endline ""
              end
              
            with
            | Sys_error msg ->
                Printf.fprintf stderr "File error: %s\n" msg
            | e ->
                remove out_name ;
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

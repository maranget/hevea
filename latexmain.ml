open Parse_opts

let finalize () =
  Image.finalize () ;
  Html.finalize ()
;;

let read_style name =
  try
    let name,chan =  Myfiles.open_tex name in
    if !verbose > 0 then begin
       prerr_endline ("read_style: "^name)
    end ;
    let buf = Lexing.from_channel chan in
    Location.set name buf ;
    Latexscan.main buf ;
    Location.restore ()
  with Myfiles.Except-> ()
;;
 
let main () = 

    Save.silent := !Parse_opts.silent ;

    if !readverb > 0 then Parse_opts.verbose := 1;
    read_style "hevea.sty" ;

    begin match !files with
     [] -> files := ["article.sty"]
    | _ -> () end;

    let texfile = match !files with
      [] -> ""
    | x::rest ->
       if Filename.check_suffix x ".tex" then begin
         files := rest ;
         x
       end else "" in

    Image.base :=
     (if !outname <> ""  then  Filename.chop_suffix !outname ".html"
     else begin match texfile with
        "" -> !Image.base
     | _   -> Filename.chop_suffix texfile ".tex" end) ;

    let rec do_rec = function
      [] -> ()
    | x::rest ->
       do_rec rest ;
       read_style x in

    do_rec !files ;

    let basename = match texfile with "" -> "zorglub"
      | _ -> Filename.chop_suffix texfile ".tex" in

    Location.set_base basename ;

    Latexscan.out_file :=
      if !outname <> "" then
         Out.create_chan (open_out !outname)
      else begin match texfile with
        "" ->  Out.create_chan stdout
      | s  ->
         Out.create_chan
           (open_out (Filename.basename basename^".html")) end ;
        
    begin match texfile with
      "" -> Latexscan.no_prelude ()
    | _  ->
       let auxname = Filename.basename basename^".aux" in
       try
         let _,auxchan = Myfiles.open_tex auxname in
         let buf = Lexing.from_channel auxchan in
         Aux.main buf
       with Myfiles.Error _ -> begin
         if !verbose > 0 then
           prerr_endline ("Cannot open aux file: "^auxname)
       end
    end ;
    let chan = match texfile with "" -> stdin | _ -> open_in texfile in
    let buf = Lexing.from_channel chan in
    Location.set texfile buf ;
    verbose := !readverb;
    Latexscan.main buf ;
    Location.restore () ;
    finalize ()
;;   


begin try
  main ()
with x -> begin
  Location.print_pos () ;
  prerr_endline "Adios" ;
  raise x
  end
end ;
exit 0;;


let verbose = ref 0
;;

let set_verbose () =
  Image.verbose := !verbose ;
  Html.verbose := !verbose ;
  Latexscan.verbose := !verbose;
  Latexmacros.verbose := !verbose
;;

let files = ref []
;;

let add_input s =
  files := s :: !files
;;

let finalize () =
  Image.finalize () ;
  Out.close !Latexscan.out_file
;;

let read_style name =
  let chan =  open_in name in
  let buf = Lexing.from_channel chan in
  Location.set name buf ;
  Latexscan.main buf ;
  Location.restore ()
;;
 
let main () =
  try begin  Arg.parse
    [("-v", Arg.Unit (fun () -> verbose := !verbose + 1),
       "verbose flag, can be repeated to increase verbosity") ;
     ("-e", Arg.String Myfiles.erecord,
       "-e filename, prevents filename from being read")
    ]
    (add_input)
    "htmlgen 0.00" ;

    set_verbose ();
    read_style "/usr/local/lib/htmlgen/htmlgen.sty" ;

    let texfile = match !files with
      [] -> ""
    | x::rest ->
       if Filename.check_suffix x ".tex" then begin
         files := rest ;
         x
       end else "" in

    Image.base := begin match texfile with
      "" -> !Image.base
    | _   -> Filename.chop_suffix texfile ".tex" end ;

    let rec do_rec = function
      [] -> ()
    | x::rest ->
       do_rec rest ;
       read_style x in

    do_rec !files ;

    Latexscan.out_file := begin match texfile with
      "" ->  Out.create_chan stdout
    | _   -> Out.create_chan
        (open_out (Filename.chop_suffix texfile ".tex"^".html")) end ;

    begin match texfile with
      "" -> ()
    | _  ->
       let auxname = Filename.chop_suffix texfile ".tex"^".aux" in
       try
         let auxchan = open_in auxname in
         let buf = Lexing.from_channel auxchan in
         Aux.main buf
       with Sys_error _ -> begin
         if !verbose > 0 then
           prerr_endline ("Cannot open aux file: "^auxname)
       end
    end ;
    let chan = match texfile with "" -> stdin | _ -> open_in texfile in
    let buf = Lexing.from_channel chan in
    Location.set texfile buf ;
    Image.start () ;
    Latexscan.main buf ;
    Location.restore () ;
    finalize ()
  end with x -> begin
    Location.print_pos () ;
    prerr_endline "Good bye" ;
    finalize () ; raise x
  end
;;   


begin try
  main ()
with x -> begin
  Location.print_pos () ;
  raise x
  end
end ;
exit 0;;

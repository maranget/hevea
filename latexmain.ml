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
       "verbose flag, can be repeated to increase verbosity")
    ]
    (add_input)
    "htmlgen 0.00" ;

    set_verbose ();
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

    let chan = match texfile with "" -> stdin | _ -> open_in texfile in
    let buf = Lexing.from_channel chan in
    Location.set texfile buf ;
    Latexscan.main buf ;
    Location.restore () ;
    finalize ()
  end with x -> finalize () ; raise x
;;   


begin try
  Printexc.print main ()
with x -> begin
  Location.print_pos () ;
  raise x
  end
end ;
exit 0;;

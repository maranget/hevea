let filename = ref ""
;;

let outname = ref "index.html"
;;
  
let main () =
  Arg.parse
    [("-o", Arg.String (fun s -> outname := s),
       "filename, make htmlcut output go into file ``filename'' (defaults to index.html)");
     ("-v", Arg.Unit (fun () -> incr Cut.verbose),
        ", verbose flag")    ]
     (fun s -> filename := s) ("hacha "^Version.version);
  Cut.name := Filename.chop_extension !filename ;
  let chan = open_in !filename in
  let buf = Lexing.from_channel chan in
  Location.set !filename buf ;
  Cut.start_phase !outname ;
  Cut.main buf ;
  incr Cut.phase ;
  let chan = open_in !filename in
  let buf = Lexing.from_channel chan in
  Location.set !filename buf ;
  Cut.start_phase !outname ;
  Cut.main buf
;;


let _ = try
  main () ;
  Mylib.copy_from_lib "previous_motif.gif" ;  
  Mylib.copy_from_lib "next_motif.gif" ;  
  Mylib.copy_from_lib "contents_motif.gif" 
with x -> begin
  Location.print_pos () ;
  prerr_endline "Adios" ;
  raise x
end
;;

exit 0;;

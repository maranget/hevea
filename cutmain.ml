let filename = ref ""
;;

let outname = "index.html"
;;


  
let main () =
  Arg.parse [] (fun s -> filename := s) "htmlcut 0.0" ;
  Cut.name := Filename.chop_extension !filename ;
  let chan = open_in !filename in
  let buf = Lexing.from_channel chan in
  Location.set !filename buf ;
  Cut.start_phase outname ;
  Cut.main buf ;
  incr Cut.phase ;
  let chan = open_in !filename in
  let buf = Lexing.from_channel chan in
  Location.set !filename buf ;
  Cut.start_phase outname ;
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

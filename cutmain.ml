let filename = ref ""
;;

let main () =
  Arg.parse [] (fun s -> filename := s) "htmlcut 0.0" ;
  Cut.name := Filename.chop_extension !filename ;
  let chan = open_in !filename in
  let buf = Lexing.from_channel chan in
  Location.set !filename buf ;
  Cut.main buf ;
  incr Cut.phase ;
  Cut.start_phase () ;
  let chan = open_in !filename in
  let buf = Lexing.from_channel chan in
  Location.set !filename buf ;
  Cut.main buf
;;


let _ = try
  main () ;
with x -> begin
  Location.print_pos () ;
  prerr_endline "Adios" ;
  raise x
end
;;

exit 0;;

let verbose = ref 0
;;

let files = ref []
;;

let add_input s =
  files := s :: !files
;;

type language = Francais | English
;;

let language = ref English
and symbols = ref true
;;

let read_idx = ref false
;;

let except = ref []
;;

let path = ref []
;;


let _ = Arg.parse
    [("-v", Arg.Unit (fun () -> verbose := !verbose + 1),
       ", verbose flag, can be repeated to increase verbosity") ;
     ("-e", Arg.String (fun s -> except := s :: !except),
       "``filename'' prevents file ``filename'' from being read") ;
     ("-idx",Arg.Unit (fun () -> read_idx := true),
       ", attempt to read .idx file (useful if indexing is non-standard)") ;
     ("-francais",Arg.Unit (fun () -> language := Francais),
       ",s et french mode") ;
     ("-nosymb",Arg.Unit (fun () -> symbols := false),
       ", do not output symbol fonts") ;
     ("-I", Arg.String (fun s -> path := s :: !path),
       "dir, add dir to search path")
    ]
    (add_input)
   "htmlgen 0.0"
;;


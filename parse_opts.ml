let verbose = ref 0
and readverb = ref 0
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

let outname = ref ""
;;

let _ = Arg.parse
    [("-v", Arg.Unit (fun () -> readverb := !readverb + 1),
       ", verbose flag, can be repeated to increase verbosity") ;
     ("-e", Arg.String (fun s -> except := s :: !except),
       "filename, prevent file ``filename'' from being read") ;
     ("-idx",Arg.Unit (fun () -> read_idx := true),
       ", attempt to read .idx file (useful if indexing is non-standard)") ;
     ("-francais",Arg.Unit (fun () -> language := Francais),
       ", french mode") ;
     ("-nosymb",Arg.Unit (fun () -> symbols := false),
       ", do not output symbol fonts") ;
     ("-I", Arg.String (fun s -> path := s :: !path),
       "dir, add directory ``dir'' to search path") ;
     ("-o", Arg.String (fun s -> outname := s),
       "filename, make hevea output go into file ``filename''")
    ]
    (add_input)
   "hevea 0.0"
;;


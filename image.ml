let verbose = ref 0
;;

let base = ref "image"
;;

let count = ref 0
;;

let buff = ref (Out.create_chan (open_out "/dev/null"))
;;

let start () =
  buff := Out.create_buff ()
;;


let put s = Out.put !buff s
and put_char c = Out.put_char !buff c
;;

let open_chan () =
  let chan = open_out (!base^".image.tex") in
  Out.to_chan chan !buff ;
  buff := Out.create_chan chan ;
  Out.put !buff "\\pagestyle{empty}\n";
  Out.put !buff "\\begin{document}\n"

and close_chan () =
  Out.put !buff "\\end{document}\n" ;
  Out.close !buff
;;


let my_string_of_int n =
  let r0 = n mod 10 and q0 = n / 10 in
  let r1 = q0 mod 10 and q1 = q0 / 10 in
  let r2 = q1 mod 10 in
  string_of_int r2^string_of_int r1^string_of_int r0
;;


let lastpage = ref 0
;;

let page () =
  incr lastpage ;
  !base^my_string_of_int !lastpage^".gif"
;;

let open_image () =
  let n = !count in
  if !verbose > 0 then
    Printf.fprintf stderr "dump image number %d\n" (n+1) ;
  (if n = 0 then
    open_chan()) ;
  count := !count + 1;
  Out.put !buff ".PS\n" ;

and close_image () =
  Out.put !buff ".PE\n";
;;

let dump image lexbuf =
  open_image ();
  image lexbuf
;;

let finalize () =  close_chan()
;;

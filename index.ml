
let table = Hashtbl.create 17
;;

let count = ref 0
;;


let treat lexbuf =
  let rec get_rec () =
    try
      let me = Entry.entry lexbuf in
      me::get_rec ()
    with Entry.Over (a,b) -> [a,b] in
  let l = get_rec () in
  Html.loc_ref ("@"^string_of_int !count) "" ;
  Hashtbl.add table l !count ;
  prerr_endline ("Index: "^string_of_int !count) ;
  count := !count + 1
;;

      

open Parse_opts

let rec comp l1 l2 = match l1,l2 with
  [],[] -> 0
| [],_  -> -1
| _,[]  -> 1
| (x1,_)::r1,(x2,_)::r2 ->
     let t = compare x1 x2 in
     if t=0 then comp r1 r2 else t
;;

type key = string list * string * string
;;

let  pretty_one_key = function
  (a,"") -> a
| (a,b)  -> a^"@"^b
;;

let rec pretty_key  = function
  [] -> ""
| [c] -> pretty_one_key c
| c::rest -> pretty_one_key c^"!"^pretty_key rest
;;



let read_index lexbuf =
  let rec get_rec () =
    try
      let me = Entry.entry lexbuf in
      let s1,s2 = me in
      me::get_rec ()
    with Entry.Over (a,b) ->  [a],b in
  get_rec ()
;;

module OrderedKey = struct
  type t = key
  let compare = comp
end
;;

module KeySet =  Set.Make(OrderedKey)
;;

open KeySet


type idx = No | Yes of key array
;;

let (itable:
   (string,
   string * KeySet.t ref * (key, (string * int)) Hashtbl.t * int ref * idx)
   Hashtbl.t) = Hashtbl.create 17
;;


let newindex tag suf name =
  let basename = Location.get_base () in
  let filename = basename^"."^suf in
  let idxstruct =
    if !read_idx then begin
      try      
        let fullname,chan = Myfiles.open_tex filename in
        let lexbuf = Lexing.from_channel chan in
        let rec do_rec () = try
            let arg = Entry.idx lexbuf in
            let k = read_index (Lexing.from_string arg) in
            k::do_rec ()
          with Entry.Fini -> [] in
        let r = do_rec () in
        if !verbose > 0 then
          prerr_endline ("Index file: "^fullname^" succesfully read");
        Yes (Array.of_list r)     
      with  Myfiles.Error msg -> begin
        prerr_endline ("Index: "^msg^", I try to manage") ;
        No end
      | Myfiles.Except -> begin
        if !verbose > 0 then
          prerr_endline ("Not reading file: "^filename);
        No end      
      end else No in
  let all = ref empty
  and table = Hashtbl.create 17
  and count = ref 0 in
  Hashtbl.add itable tag (name,all,table,count,idxstruct)
;;


let treat tag arg =
  try
    if !verbose > -1 then prerr_endline ("Index.treat with arg: "^arg) ;
    let name,all,table,count,idxstruct = Hashtbl.find itable tag in
    let lexbuf = Lexing.from_string arg in
    let key = read_index lexbuf in
    let key = match idxstruct with
      No -> key
    | Yes t ->
       if !count >= Array.length t then key
       else
         let nkey = t.(!count) in
         if fst nkey = fst key then nkey else key in
    if !verbose > -1 then
      prerr_endline ("Treat index with key : "^pretty_key key) ;
    let label = ("@"^tag^string_of_int !count) in
    Html.loc_name label "" ;
    Hashtbl.add table key (label,!count) ;
    all := add key !all ;
    count := !count + 1
  with Not_found -> begin
    Location.print_pos () ;
    prerr_endline ("Index: "^tag^" is undefined, makeindex or newindex is missing")
  end
;;

      
let rec common e1 e2 = match e1,e2 with
  [],_  -> [],e2
| _,[]  -> e1,[]
| x1::r1,x2::r2 ->
    if x1=x2 then
      common r1 r2
    else
      e1,e2
;;
let rec close_prev = function
  []| [_] -> ()
| _::r    ->  Html.close_block "UL" ; close_prev r
;;

let rec open_this key main = function
  [] -> ()
| (k,t)::r ->
    let t = match t with "" -> k | _ -> t in
    Html.delay (fun _ -> ()) ;
    Html.item
      (fun tag ->
        try main (tag^"{}") ; let _ = Html.flush () in () with
        x -> begin
          Html.forget () ;
          Location.print_pos () ;
          prerr_endline ("Something wrong with index: "^tag) ;
          raise x
      end) t ;
    begin match r with
      [] -> ()
    | _  -> Html.open_block "UL" "" 
    end ;
    open_this key main r
;;


let print_entry main prev this xs  =
  let rp,rt = common prev this in
  close_prev rp ;
  begin match rp with [] ->  Html.open_block "UL" "" | _ -> () end ;
  open_this this main rt ;
  List.iter
   (fun (label,x) ->
      Html.put " " ;
      Html.loc_ref (string_of_int x) label) (List.rev xs)
;;

     
    
let print main  tag =
  let name,all,table,_,_ = Hashtbl.find itable tag in
  main ("\\@indexsection{"^name^"}") ;
  let prev = ref [] in
  KeySet.iter (fun k ->
      print_entry main !prev k (Hashtbl.find_all table k) ;
      prev := k)
 !all ;
 List.iter (fun _ -> Html.close_block "UL") !prev
;;

  


let rec comp l1 l2 = match l1,l2 with
  [],[] -> 0
| [],_  -> -1
| _,[]  -> 1
| (x1,_)::r1,(x2,_)::r2 ->
     let t = compare x1 x2 in
     if t=0 then comp r1 r2 else t
;;

module OrderedKey = struct
  type t = (string * string) list
  let compare = comp
end
;;

module KeySet =  Set.Make(OrderedKey)
;;

open KeySet

let itable = Hashtbl.create 17
;;

let newindex tag name =
  let all = ref empty
  and table = Hashtbl.create 17
  and count = ref 0 in
  Hashtbl.add itable tag (name,all,table,count)
;;


let treat tag lexbuf =
  let name,all,table,count = Hashtbl.find itable tag in
  let rec get_rec () =
    try
      let me = Entry.entry lexbuf in
      me::get_rec ()
    with Entry.Over (a,b) -> [a,b] in
  Entry.skip_par lexbuf ;
  let key = get_rec () in
  let label = ("@"^tag^string_of_int !count) in
  Html.loc_name label "" ;
  Hashtbl.add table key (label,!count) ;
  all := add key !all ;
  count := !count + 1
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

let rec open_this main = function
  [] -> ()
| (k,t)::r ->
    let t = match t with "" -> k | _ -> t in
    Html.item (fun () -> main t) ;
    begin match r with
      [] -> ()
    | _  -> Html.open_block "UL" "" 
    end ;
    open_this main r
;;


let print_entry main prev this xs  =
  let rp,rt = common prev this in
  close_prev rp ;
  begin match rp with [] ->  Html.open_block "UL" "" | _ -> () end ;
  open_this main rt ;
  List.iter
   (fun (label,x) ->
      Html.put " " ;
      Html.loc_ref (string_of_int x) label) (List.rev xs)
;;

     
    
let print main  tag =
  let name,all,table,_ = Hashtbl.find itable tag in
  Html.open_block "H2" "" ;
  main name ;
  Html.close_block "H2" ;
  let prev = ref [] in
  KeySet.iter (fun k ->
    print_entry main !prev k (Hashtbl.find_all table k) ;
    prev := k)
 !all ;
 List.iter (fun _ -> Html.close_block "UL") !prev
;;

  

type 'a t = {mutable l : 'a list ; name : string}


let create name = {l = [] ; name=name}

and push s x = s.l <- x :: s.l

and pop s = match s.l with
| [] -> raise (Misc.Fatal ("Empty stack (pop): "^s.name))
| x :: r ->
    s.l <- r ;
    x

and top s = match s.l with
| [] -> raise (Misc.Fatal ("Empty stack (top): "^s.name))
| x :: _ -> x

and length s = List.length s.l

and empty s = match s.l with
| [] -> true
| _  -> false

let pretty f stack =
  prerr_string stack.name ;
  prerr_string ": <<" ;
  let rec do_rec = function
    | [] -> prerr_endline ">>"
    | [x] ->
        prerr_string ("``"^f x^"''") ;
        prerr_endline ">>"
    | x :: r ->
        prerr_string "``" ;
        prerr_string (f x) ;
        prerr_string "'' " ;
        do_rec r in
  do_rec stack.l

let rev s = s.l <- List.rev s.l

type 'a saved = 'a list

let empty_saved = []
and save {l=l} = l
and restore s x = s.l <- x

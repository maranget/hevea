type t = {mutable t : string ; mutable p : int}
;;

let create () = {t = String.create 64 ; p = 0}

let rec realloc d b =
  let l = String.length b.t in
  if b.p + d-1 >= l then begin
    let new_t = String.create (2*l) in
    String.blit b.t 0 new_t 0 b.p ;
    b.t <- new_t  ;
    realloc d b 
  end


let put_char b c =
  realloc 1 b ;
  b.t.[b.p] <- c ;
  b.p <- b.p + 1

let put b s =
  let l = String.length s in
  realloc l b ;
  String.blit s 0 b.t b.p l ;
  b.p <- b.p + l

let to_string b =
  let r = String.sub b.t 0 b.p in
  b.p <- 0 ;
  r
  

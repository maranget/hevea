type 'a t = {mutable next : int ; mutable data : 'a array}

let default_size = 32
;;

let create x = {next = 0 ; data = Array.create default_size x}
and reset t = t.next <- 0
;;

let incr_table table new_size =
  let t = Array.create new_size table.data.(0) in
  Array.blit table.data 0 t 0 (Array.length table.data) ;
  table.data <- t

let emit table i =
 let size = Array.length table.data in
 if table.next >= size then
    incr_table table (2*size);
 table.data.(table.next) <- i ;
 table.next <- table.next + 1


let apply table f =
  if table.next = 0 then
    raise (Failure "Table.apply") ;
  f table.data.(table.next - 1)

let trim t =
  let r = Array.sub t.data 0 t.next in
  reset t ;
  r

let remove_last table =
  table.next <- table.next -1;
  if table.next < 0 then table.next <- 0 ;
;;

let get_size table = table.next
;;


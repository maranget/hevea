type 'a t
val create : string ->  'a t
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a
val top : 'a t -> 'a
val pretty : ('a -> string) -> 'a t -> unit
val length : 'a t -> int
val empty : 'a t -> bool
val rev : 'a t -> unit

type 'a saved
val empty_saved : 'a saved
val save : 'a t -> 'a saved
val restore : 'a t -> 'a saved -> unit

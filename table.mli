type 'a t

val create : 'a -> 'a t
val reset : 'a t -> unit

val emit : 'a t -> 'a -> unit
val apply : 'a t -> ('a -> unit) -> unit
val trim : 'a t -> 'a array
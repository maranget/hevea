val cost : int * int -> Htmltext.style Tree.t -> int * int
val costs : int * int -> Htmltext.style Tree.t list -> int * int
val there : Htmltext.t_style -> Htmltext.style -> bool
val inter : Htmltext.style -> Htmltext.style -> Htmltext.style
val sub : Htmltext.style -> Htmltext.style -> Htmltext.style
val neutral : Htmltext.style -> Htmltext.style * Htmltext.style
val is_blank : 'a Tree.t -> bool
val is_blanks : 'a Tree.t list -> bool
val nodes :
  Htmltext.style -> Htmltext.style Tree.t list ->  Htmltext.style Tree.t list
val node :
  Htmltext.style -> Htmltext.style Tree.t list ->  Htmltext.style Tree.t


module type Config = sig
  type t
  val of_string : string -> t
  val of_out : Out.t -> t
end


module type S = sig
  type out
  val opt : Lexing.lexbuf -> out
  val arg : Lexing.lexbuf -> out
  val arg2 : Lexing.lexbuf -> out
end

module Make(C:Config) : S with type out = C.t
  
module String : S with type out = string
module List : S with type out = string list

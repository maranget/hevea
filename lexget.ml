module type S =
  sig
  val get_this_arg :
        (Lexing.lexbuf -> unit) -> string * Lexstate.subst -> string
      val get_this_nostyle_arg :
        (Lexing.lexbuf -> unit) -> string * Lexstate.subst -> string
      val get_this : (Lexing.lexbuf -> unit) -> string -> string
      val get_this_nostyle :
        (Lexing.lexbuf -> unit) -> string -> string
      val get_this_clearstyle :
        (Lexing.lexbuf -> unit) -> string -> string
  end

module Make (Dest:OutManager.S) =
struct
open Parse_opts
open Misc
open Lexstate

let do_get_this make_style  lexfun (s,subst) =
  let par_val = Dest.forget_par () in
  start_normal subst ;

  if !verbose > 1 then
    prerr_endline ("get_this : ``"^s^"''") ;  
  let lexer = Lexing.from_string s in
  let r = Dest.to_string (fun () ->
(*    top_open_group () ; *)
    make_style () ;
    lexfun lexer ;
(*    top_close_group () *)) in

  if !verbose > 1 then begin
    prerr_endline ("get_this ``"^s^"'' -> ``"^r^"''")
  end ;
  end_normal () ;
  Dest.par par_val ;
  r


let get_this_arg lexfun s = do_get_this (fun () -> ()) lexfun s
and get_this_nostyle_arg lexfun s = do_get_this Dest.nostyle lexfun s

let get_this lexfun s = do_get_this (fun () -> ()) lexfun (s,get_subst ())
and get_this_nostyle lexfun s =
  do_get_this Dest.nostyle lexfun (s,get_subst ())
and get_this_clearstyle lexfun s =
  do_get_this Dest.clearstyle lexfun (s,get_subst ())

end



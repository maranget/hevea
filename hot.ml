type saved =
    Lexstate.saved * Latexmacros.saved *
      Counter.saved * Color.saved * Foot.saved

let checkpoint () =
  Lexstate.checkpoint (),
  Latexmacros.checkpoint (),
  Counter.checkpoint (),
  Color.checkpoint (),
  Foot.checkpoint ()

and start (lexstate, latexmacros, counter, color, foot) =
  Misc.hot_start () ;
  Lexstate.hot_start lexstate ;
  Latexmacros.hot_start latexmacros ;
  Counter.hot_start counter ;
  Color.hot_start color ;
  Foot.hot_start foot ;
  begin match !Parse_opts.destination with
  | Parse_opts.Info -> InfoRef.hot_start ()
  | _ -> ()
  end


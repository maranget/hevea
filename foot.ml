open Parse_opts
open Html

type ok = Some of string * string * string | None

let table = ref (Array.make 10 None)
and some = ref false
;;

let register i mark text anchor =
  some := true ;
  if Array.length !table < i-1 then begin
    let t = Array.make (i*2) None in
    Array.blit !table 0 t 0 (Array.length !table) ;
    table := t
  end ;
  begin match !table.(i-1) with
    None -> ()
  | Some (_,_,_) -> begin
      Location.print_pos () ;
      prerr_endline "Warning: erasing previous footnote"
    end
  end ;
  !table.(i-1) <- Some (mark,text,anchor)
;;

let flush lexer =
  if !some then begin
    some := false ;
    lexer "\\footnoterule" ;
    Html.open_block "DL" "" ;
    let t = !table in
    for i = 0 to Array.length t - 1 do
      match t.(i) with
        None -> ()
      | Some (m,txt,anchor) ->
          t.(i) <- None ;
          Html.item (fun s ->
             lexer ("\\@openanchor{text}{note}{"^anchor^"}") ;
             Html.put s ;
             lexer ("\\@closeanchor")) m;
          Html.put txt ;
          Html.put_char '\n'
    done ;
    Html.force_block "DL" ""
  end
;;

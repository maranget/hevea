let () =
  if compare Sys.ocaml_version "4.02.0" >= 0  then
    Printf.printf "ok"
  else
    Printf.printf "no"


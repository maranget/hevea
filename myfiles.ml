let etable = Hashtbl.create 17
;;

let erecord name = Hashtbl.add etable name ()
;;

let is_except name =
  try Hashtbl.find etable name ; true with Not_found -> false
;;

let tex_path =  try
  let r = ref []
  and j = ref 0 in
  let s = Sys.getenv "TEXINPUTS" in
  for i=0 to String.length s-1 do
    match String.get s i with
     ':' ->
        r := String.sub s !j (i- !j) :: !r ;
        j := i+1
    | _  -> ()
  done ;
  r := String.sub s !j (String.length s - !j -1) :: !r ;
  List.rev !r
with Not_found -> ["." ; "/usr/local/lib/tex"]
;;

exception Found of (string * in_channel)
;;

let do_open_tex filename =
  try
    List.iter (fun dir ->
      try
        let full_name = Filename.concat dir filename in
        let r = open_in full_name in
        raise (Found (full_name,r))
      with Sys_error _ -> ())
    tex_path ;
    failwith ("Cannot open file: "^filename)
  with Found r -> r
;;

let open_tex filename =
  if is_except filename then raise Not_found ;
  try
    do_open_tex filename
  with Failure _ as x ->
    if Filename.check_suffix filename ".tex" then
      raise x
    else
      do_open_tex (filename^".tex")
;;


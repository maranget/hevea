let verbose = ref 0
;;

let etable = Hashtbl.create 17
;;

let erecord name = Hashtbl.add etable name ()
;;

let is_except name =
  try Hashtbl.find etable name ; true with Not_found -> false
;;

let tex_path = try
  let r = ref []
  and j = ref 0 in
  let s = Sys.getenv "HTMLINPUTS" in
  for i=0 to String.length s-1 do
    match String.get s i with
     ':' ->
        let d = String.sub s !j (i- !j) in
        r := d :: !r ;
        j := i+1
    | _  -> ()
  done ;
  let d = String.sub s !j (String.length s - !j) in
  r :=  d :: !r ;
  List.rev !r
with Not_found -> ["." ; "/usr/local/lib/htmlgen"]
;;

exception Found of (string * in_channel)
;;

let do_open_tex filename =
  try
    List.iter (fun dir ->
      try
        let full_name = Filename.concat dir filename in
        if !verbose > 0 then prerr_endline ("Trying: "^full_name) ;
        let r = open_in full_name in
        raise (Found (full_name,r))
      with Sys_error _ -> ())
    tex_path ;
    failwith ("Cannot open file: "^filename)
  with Found r -> r
;;

let open_tex filename =
  if is_except filename then raise Not_found ;
  if Filename.is_implicit filename then
    try
      do_open_tex filename
    with Failure _ ->
      if Filename.check_suffix filename ".tex" then
        failwith ("Cannot find: "^filename)
      else
        try do_open_tex (filename^".tex")
        with Failure _ -> failwith ("Cannot find file: "^filename)
  else
    try filename,open_in filename
    with Sys_error _ ->
      if Filename.check_suffix filename ".tex" then
        failwith ("Cannot open: "^filename)
      else
        try (filename^".tex"),open_in (filename^".tex") with
        Sys_error _ -> failwith ("Cannot open: "^filename)
;; 


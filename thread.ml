let uptable = Hashtbl.create 17
and nexttable = Hashtbl.create 17
and prevtable = Hashtbl.create 17
;;

let setup file upname = Hashtbl.add uptable file upname
;;

let setprevnext prev now =
  if prev <> "" then begin
    Hashtbl.add nexttable prev now ;
    Hashtbl.add prevtable now prev
  end
;;

let next name = Hashtbl.find nexttable name
and up   name = Hashtbl.find uptable name
and prev name = Hashtbl.find prevtable name
;;

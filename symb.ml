(***********************************************************************)
(*                                                                     *)
(*                          HEVEA                                      *)
(*                                                                     *)
(*  Luc Maranget, projet PARA, INRIA Rocquencourt                      *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

let header = "$Id: symb.ml,v 1.13 1998-10-22 09:45:24 maranget Exp $" 
open Parse_opts

exception No
;;

let get (a,b,c) =
  try
    if !Parse_opts.symbols then begin
      match a with "" -> raise No | _ -> a
    end else raise No
  with No ->  match !language with
    Francais -> c
  | English -> b
;;

let alpha = get ("<FONT FACE=symbol>\097</FONT>","alpha","alpha");;
let beta = get ("<FONT FACE=symbol>\098</FONT>","beta","beta");;
let gamma = get ("<FONT FACE=symbol>\103</FONT>","gamma","gamma");;
let delta = get ("<FONT FACE=symbol>\100</FONT>","delta","delta");;
let epsilon = get ("<FONT FACE=symbol>\101</FONT>","epsilon","epsilon");;
let zeta = get ("<FONT FACE=symbol>\122</FONT>","zeta","zeta");;
let eta = get ("<FONT FACE=symbol>\104</FONT>","eta","eta");;
let theta = get ("<FONT FACE=symbol>\113</FONT>","theta","theta");;
let vartheta = get ("<FONT FACE=symbol>\074</FONT>","vartheta","vartheta");;
let iota = get ("<FONT FACE=symbol>\105</FONT>","iota","iota");;
let kappa = get ("<FONT FACE=symbol>\107</FONT>","kappa","kappa");;
let lambda = get ("<FONT FACE=symbol>\108</FONT>","lambda","lambda");;
let mu = get ("\181","\181","\181");;
let nu = get ("<FONT FACE=symbol>\110</FONT>","nu","nu");;
let xi = get ("<FONT FACE=symbol>\120</FONT>","xi","xi");;
let pi = get ("<FONT FACE=symbol>\112</FONT>","pi","pi");;
let varpi = get ("<FONT FACE=symbol>\118</FONT>","varpi","varpi");;
let rho = get ("<FONT FACE=symbol>\114</FONT>","rho","rho");;
let varrho = get ("varrho","varrho","varrho");;
let sigma = get ("<FONT FACE=symbol>\115</FONT>","sigma","sigma");;
let varsigma = get ("<FONT FACE=symbol>\086</FONT>","varsigma","varsigma");;
let tau = get ("<FONT FACE=symbol>\116</FONT>","tau","tau");;
let upsilon = get ("<FONT FACE=symbol>\117</FONT>","upsilon","upsilon");;
let phi = get ("<FONT FACE=symbol>\102</FONT>","phi","phi");;
let varphi = get ("<FONT FACE=symbol>\106 </FONT>","varphi","varphi");;
let chi = get ("<FONT FACE=symbol>\099</FONT>","chi","chi");;
let psi = get ("<FONT FACE=symbol>\121</FONT>","psi","psi");;
let omega = get ("<FONT FACE=symbol>\119</FONT>","omega","omega");;

let upgamma = get ("<FONT FACE=symbol>\071</FONT>","Gamma","Gamma");;
let updelta = get ("<FONT FACE=symbol>\068</FONT>","Delta","Delta");;
let uptheta = get ("<FONT FACE=symbol>\081</FONT>","Theta","Theta");;
let uplambda = get ("<FONT FACE=symbol>\076</FONT>","Lambda","Lambda");;
let upxi = get ("<FONT FACE=symbol>\088</FONT>","Xi","Xi");;
let uppi = get ("<FONT FACE=symbol>\080</FONT>","Pi","Pi");;
let upsigma = get ("<FONT FACE=symbol>\083</FONT>","Sigma","Sigma");;
let upupsilon = get ("<FONT FACE=symbol>\085</FONT>","Upsilon","Upsilon");;
let upphi = get ("<FONT FACE=symbol>\070</FONT>","Phi","Phi");;
let uppsi = get ("<FONT FACE=symbol>\089</FONT>","Psi","Psi");;
let upomega = get ("<FONT FACE=symbol>\087</FONT>","Omega","Omega");;

let pm = get ("\177","\177","\177");;
let mp = get ("\177","\177","\177");;
let times = get ("\215","\215","215");;
let div = get  ("\247","\247","247");;
let ast = get ("*","*","*");;
let circ = get ("<FONT FACE=symbol>\176</FONT>","o","o");;
let bullet = get ("<FONT FACE=symbol>\183</FONT>","o","o");;
let cap = get ("<FONT FACE=symbol>\199</FONT>","inter", "inter");;
let cup = get ("<FONT FACE=symbol>\200</FONT>","U", "U");;
let sqcap = get ("<FONT FACE=symbol>\250\095\189</FONT>","sqcap", "sqcap");;
let sqcup = get ("<FONT FACE=symbol>\250\096\189</FONT>","sqcup", "sqcup");;
let vee = get ("<FONT FACE=symbol>\218</FONT>","\\/", "\\/");;
let wedge = get ("<FONT FACE=symbol>\217</FONT>","/\\", "/\\");;
let setminus = get ("\\","\\","\\");;


let bigtriangleup = get ("<FONT FACE=symbol>\068</FONT>","/_\\","/_\\");;
let bigtriangledown = get ("<FONT FACE=symbol>\209</FONT>","\\/","\\/");;
let triangleleft = get ("<FONT FACE=symbol>\060\124</FONT>","<|","<|");;
let triangleright = get ("<FONT FACE=symbol>\124\062</FONT>","|>","|>");;
let unlhd = get ("<FONT FACE=symbol>\163\124</FONT>","&lt;=|","&lt;=|");;
let unrhd = get  ("<FONT FACE=symbol>\124\179</FONT>","|&gt;=","|&gt>=");;
let oplus = get  ("<FONT FACE=symbol>\197</FONT>","oplus","plus-rond");;
let otimes = get  ("<FONT FACE=symbol>\196</FONT>","otimes","mult-rond");;

let leq = get ("<FONT FACE=symbol>\163</FONT>","&lt;=","&lt;=");;
let subset = get ("<FONT FACE=symbol>\204</FONT>","included in","inclus dans");;
let notsubset = get ("<FONT FACE=symbol>\203</FONT>","not included in","n'est pas inclus dans");;
let subseteq =
get   ("<FONT FACE=symbol>\205</FONT>","included in or equal to","inclus dans ou égal à");;
let display_sqsubset =
get  ("<FONT FACE=symbol>\190<BR>\234<BR>\190</FONT>","sqsubset", "sqsubset");;
let elem = get ("<FONT FACE=symbol>\206</FONT>","in", "appartient à");;
let geq = get ("<FONT FACE=symbol>\179</FONT>","&gt;=", "&gt;=");;
let supset = get  ("<FONT FACE=symbol>\201</FONT>","contains","contient");;
let supseteq = get  ("<FONT FACE=symbol>\202</FONT>","contains or equal to","contient ou égal à");;
let display_sqsupset = get ("<FONT FACE=symbol>\190<BR> \234<BR>\190</FONT>",
"sqsupset", "sqsupset");;
let ni = get ("<FONT FACE=symbol>\039</FONT>","contains", "contient");;


let equiv = get  ("<FONT FACE=symbol>\186</FONT>","equiv", "equiv");;
let approx = get  ("<FONT FACE=symbol>\187</FONT>","aproximates", "à peu près");;
let neq = get ("<FONT FACE=symbol>\185</FONT>","&lt;&gt;", "&lt;&gt;");;
  ("<FONT FACE=sget ymbol></FONT>","contains", "contient");;
let propto  = get ("<FONT FACE=symbol>\181</FONT>","propto", "propto");;
let perp = get  ("<FONT FACE=symbol>\094</FONT>","perp", "perp");;
let join = get  ("<FONT FACE=symbol>\241\225</FONT>","&gt;&lt;", "&gt;&lt;");;

let leftarrow = get ("<FONT FACE=symbol>\172</FONT>","&lt;-", "&lt;-");;
let upleftarrow = get ("<FONT FACE=symbol>\229</FONT>","&lt;=", "&lt;=");;
let rightarrow = get ("<FONT FACE=symbol>\174</FONT>","-&gt;", "-&gt;");;
let uprightarrow = get ("<FONT FACE=symbol>\222</FONT>","=&gt;", "=&gt;");;
let leftrightarrow = get ("<FONT FACE=symbol>\171</FONT>","&lt;-&gt;", "&lt;-&gt;");;
let upleftrightarrow = get ("<FONT FACE=symbol>\219</FONT>","&lt;=&gt;", "&lt;=&gt;");;
let longrightarrow = get ("<FONT FACE=symbol>\190\174</FONT>","--&gt;", "--&gt;");;

let int = get ("<FONT FACE=symbol>\242</FONT>","int", "integrale");;
let display_int = get ("<FONT FACE=symbol>\243<BR>\245</FONT>",
"/<BR>|<BR>/", "/<BR>|<BR>/");;

let aleph = get ("<FONT FACE=symbol>À</FONT>","aleph","aleph");;
let wp =  get ("<FONT FACE=symbol>Ã</FONT>","wp","wp");;
let upre = get ("<FONT FACE=symbol>Â</FONT>","Re","Re");;
let upim = get ("<FONT FACE=symbol>Á</FONT>","Im","Im");;
let prim = get ("<FONT FACE=symbol>¢</FONT>","'","'");;
let nabla =  get ("<FONT FACE=symbol>Ñ</FONT>","nabla","nabla");;
let surd =  get ("<FONT FACE=symbol>Ö</FONT>","surd","surd");;
let angle = get ("<FONT FACE=symbol>Ð</FONT>","angle","angle");;
let exists = get ("<FONT FACE=symbol>\036</FONT>","exists", "il existe");;
let forall = get ("<FONT FACE=symbol>\034</FONT>","for all", "pour tout");;
let partial = get ("<FONT FACE=symbol>¶</FONT>","partial", "d rond");;
let diamond = get ("<FONT FACE=symbol>à</FONT>","&lt;&gt;", "&lt;&gt;");;
let clubsuit = get ("<FONT FACE=symbol>§</FONT>","clubsuit", "trèfle");;
let diamondsuit = get ("<FONT FACE=symbol>¨</FONT>","diamondsuit", "carreau");;
let heartsuit = get ("<FONT FACE=symbol>©</FONT>","heartsuit", "coeur");;
let spadesuit = get ("<FONT FACE=symbol>ª</FONT>","spadesuit", "pique");;
let infty = get ("<FONT FACE=symbol>\165</FONT>","oo", "oo");;



let notin  = get ("<FONT FACE=symbol>\207</FONT>","not in", "n'appartient pas à");;

let uparrow =
get   ("<FONT FACE=symbol>\173</FONT>","uparrow","flèche en haut")
and upuparrow =
get  ("<FONT FACE=symbol>\221</FONT>","Uparrow","double flèche en haut")
and downarrow =
get  ("<FONT FACE=symbol>\175</FONT>","downarrow","flèche en bas")
and updownarrow =
get  ("<FONT FACE=symbol>\223</FONT>","Downarrow","double flèche en bas")
;;

let oplus =
get   ("<FONT FACE=symbol>\197</FONT>","oplus","oplus")
and otimes =
get  ("<FONT FACE=symbol>\196</FONT>","otimes","otimes")
and ominus =
get  ("<FONT FACE=symbol>\081</FONT>","ominus","ominus")
;;

let lfloor = get ("<FONT FACE=symbol>\235</FONT>","|_", "|_");;
let rfloor = get ("<FONT FACE=symbol>\251</FONT>","_|", "_|");;
let lceil = get ("<FONT FACE=symbol>\233</FONT>","|", "|");;
let rceil = get ("<FONT FACE=symbol>\249</FONT>","|", "|");;
let langle = get ("<FONT FACE=symbol>\225</FONT>","&lt;", "&lt;");;
let rangle = get ("<FONT FACE=symbol>\241</FONT>","&gt;", "&gt;");;


let tr = function
  "<" -> "&lt;"
| ">" -> "&gt;"
| "\\{" -> "{"
| "\\}" -> "}"
| s   -> s
;;
let put_delim skip put d n =

  let  put_skip s = put s ; skip () ; in

  let rec do_rec s i =
    if i >= 1 then begin
      put_skip s;
      do_rec s (i-1)
    end

  and do_bis s i =
    if i>= 2 then begin
      put_skip s ;
      do_bis s (i-1)
    end else
      put s in

  if not !symbols || n=1 then
    let d = tr d in
    do_bis d n
  else begin
    put "<FONT FACE=symbol>\n" ;
    if d = "(" then begin
      put_skip "æ" ;
      do_rec "ç" (n-2) ;
      put "è"
    end else if d=")" then begin
      put_skip "ö" ;
      do_rec "÷" (n-2) ;
      put "ø"
    end else if d = "[" then begin
      put_skip "é" ; 
      do_rec "ê" (n-2) ;
      put "ë"
    end else if d="]" then begin
      put_skip "ù" ; 
      do_rec "ú" (n-2) ;
      put "û"
   end else if d = "\\lfloor" then begin
      do_rec "ê" (n-1) ;
      put "ë"
    end else if d="\\rfloor" then begin
      do_rec "ú" (n-1) ;
      put "û"
    end else if d = "\\lceil" then begin
      put_skip "é" ; 
      do_bis "ê" (n-1)
    end else if d="\\rceil" then begin
      put_skip "ù" ; 
      do_bis "ú" (n-1)
    end else if d="|" then begin
      do_bis "½" n
    end else if d="\\|" then begin
      do_bis "½½" n
    end else if d = "\\{" then begin
      put_skip "ì" ; 
      do_rec "ï" ((n-3)/2) ;
      put_skip "í" ; 
      do_rec "ï" ((n-3)/2) ;
      put "î"     
    end else if d = "\\}" then begin
      put_skip "ü" ; 
      do_rec "ï" ((n-3)/2) ;
      put_skip "ý" ; 
      do_rec "ï" ((n-3)/2) ;
      put "þ"     
    end ;
    put "</FONT>"
  end
;;


   
  

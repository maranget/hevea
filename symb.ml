type t = Symbol | Francais | English
;;

let now = ref Symbol
;;

let get (a,b,c) = match !now with
  Symbol -> a
| Francais -> b
| English -> c
;;

let alpha = ("<FONT FACE=symbol>\097</FONT>","alpha","alpha");;
let beta = ("<FONT FACE=symbol>\098</FONT>","beta","beta");;
let gamma = ("<FONT FACE=symbol>\103</FONT>","gamma","gamma");;
let delta = ("<FONT FACE=symbol>\100</FONT>","delta","delta");;
let epsilon = ("<FONT FACE=symbol>\101</FONT>","epsilon","epsilon");;
let varepsilon = ("varepsilon","varepsilon","varepsilon");;
let zeta = ("<FONT FACE=symbol>\122</FONT>","zeta","zeta");;
let eta = ("<FONT FACE=symbol>\104</FONT>","eta","eta");;
let theta = ("<FONT FACE=symbol>\113</FONT>","theta","theta");;
let vartheta = ("<FONT FACE=symbol>\074</FONT>","vartheta","vartheta");;
let iota = ("<FONT FACE=symbol>\105</FONT>","iota","iota");;
let kappa = ("<FONT FACE=symbol>\107</FONT>","kappa","kappa");;
let lambda = ("<FONT FACE=symbol>\108</FONT>","lambda","lambda");;
let mu = ("\181","\181","\181");;
let nu = ("<FONT FACE=symbol>\110</FONT>","nu","nu");;
let xi = ("<FONT FACE=symbol>\120</FONT>","xi","xi");;
let pi = ("<FONT FACE=symbol>\112</FONT>","pi","pi");;
let varpi = ("<FONT FACE=symbol>\118</FONT>","varpi","varpi");;
let rho = ("<FONT FACE=symbol>\114</FONT>","rho","rho");;
let varrho = ("varrho","varrho","varrho");;
let sigma = ("<FONT FACE=symbol>\115</FONT>","sigma","sigma");;
let varsigma = ("<FONT FACE=symbol>\086</FONT>","varsigma","varsigma");;
let tau = ("<FONT FACE=symbol>\116</FONT>","tau","tau");;
let upsilon = ("<FONT FACE=symbol>\117</FONT>","upsilon","upsilon");;
let phi = ("<FONT FACE=symbol>\102</FONT>","phi","phi");;
let varphi = ("<FONT FACE=symbol>\106 </FONT>","varphi","varphi");;
let chi = ("<FONT FACE=symbol>\99</FONT>","chi","chi");;
let psi = ("<FONT FACE=symbol>\121</FONT>","psi","psi");;
let omega = ("<FONT FACE=symbol>\119</FONT>","omega","omega");;

let upgamma = ("<FONT FACE=symbol>\071</FONT>","Gamma","Gamma");;
let updelta = ("<FONT FACE=symbol>\068</FONT>","Delta","Delta");;
let uptheta = ("<FONT FACE=symbol>\081</FONT>","Theta","Theta");;
let uplambda = ("<FONT FACE=symbol>\076</FONT>","Lambda","Lambda");;
let upxi = ("<FONT FACE=symbol>\088</FONT>","Xi","Xi");;
let uppi = ("<FONT FACE=symbol>\080</FONT>","Pi","Pi");;
let upsigma = ("<FONT FACE=symbol>\083</FONT>","Sigma","Sigma");;
let upupsilon = ("<FONT FACE=symbol>\085</FONT>","Upsilon","Upsilon");;
let upphi = ("<FONT FACE=symbol>\070</FONT>","Phi","Phi");;
let uppsi = ("<FONT FACE=symbol>\089</FONT>","Psi","Psi");;
let upomega = ("<FONT FACE=symbol>\087</FONT>","Omega","Omega");;

let pm = ("\177","\177","\177");;
let mp = ("\177","\177","\177");;
let times = ("\215","\215","215");;
let div =  ("\247","\247","247");;
let ast = ("*","*","*");;
let star = ("star","star","étoile");;
let circ = ("<FONT FACE=symbol>\176</FONT>","o","o");;
let bullet = ("<FONT FACE=symbol>\183</FONT>","o","o");;
let cdot = ("<FONT FACE=symbol>\215</FONT>",".",".");;
let cap = ("<FONT FACE=symbol>\199</FONT>","inter", "inter");;
let cup = ("<FONT FACE=symbol>\200</FONT>","U", "U");;
let sqcap = ("<FONT FACE=symbol>\250\095\189</FONT>","sqcap", "sqcap");;
let sqcup = ("<FONT FACE=symbol>\250\096\189</FONT>","sqcup", "sqcup");;
let vee = ("<FONT FACE=symbol>\218</FONT>","\\/", "\\/");;
let wedge = ("<FONT FACE=symbol>\217</FONT>","/\\", "/\\");;
let setminus = ("\\","\\","\\");;
let wr = ("wr","wr","wr");;
let diamond = ("<FONT FACE=symbol>\224</FONT>","<>","<>");;
let bigtriangleup = ("<FONT FACE=symbol>\068</FONT>","/_\\","/_\\");;
let bigtriangledown = ("<FONT FACE=symbol>\209</FONT>","\\/","\\/");;
let triangleleft = ("<FONT FACE=symbol>\060\124</FONT>","<|","<|");;
let triangleright = ("<FONT FACE=symbol>\124\062</FONT>","|>","|>");;
let unlhd = ("<FONT FACE=symbol>\163\124</FONT>","<=|","<=|");;
let unrhd =  ("<FONT FACE=symbol>\124\179</FONT>","|>=","|>=");;
let oplus =  ("<FONT FACE=symbol>\197</FONT>","oplus","plus-rond");;
let otimes =  ("<FONT FACE=symbol>\196</FONT>","otimes","mult-rond");;

let leq = ("<FONT FACE=symbol>\163</FONT>","<=","<=");;
let subset = ("<FONT FACE=symbol>\204</FONT>","included in","inclus dans");;
let subseteq =
  ("<FONT FACE=symbol>\204</FONT>","included in or equal to","inclus dans ou égal à");;
let display_sqsubset =
 ("<FONT FACE=symbol>\190<BR>\234<BR>\190</FONT>","sqsubset", "sqsubset");;
let elem = ("<FONT FACE=symbol>\206</FONT>","in", "appartient à");;
let geq = ("<FONT FACE=symbol>\179</FONT>",">=", ">=");;
let supset =  ("<FONT FACE=symbol>\201</FONT>","contains","contient");;
let supseteq =  ("<FONT FACE=symbol>\202</FONT>","contains or equal to","contient ou égal à");;
let display_sqsupset = ("<FONT FACE=symbol>\190<BR> \234<BR>\190</FONT>",
"sqsupset", "sqsupset");;
let ni = ("<FONT FACE=symbol>\039</FONT>","contains", "contient");;


let equiv =  ("<FONT FACE=symbol>\186</FONT>","equiv", "equiv");;
let approx =  ("<FONT FACE=symbol>\187</FONT>","aproximates", "à peu près");;
let cong =  ("cong","cong", "congru à");;
let neq = ("<FONT FACE=symbol>\185</FONT>","&lt;&gt;", "&lt;&gt;");;
  ("<FONT FACE=symbol></FONT>","contains", "contient");;
let propto  = ("<FONT FACE=symbol>\181</FONT>","propto", "propto");;
let perp =  ("<FONT FACE=symbol>\094</FONT>","perp", "perp");;
let join =  ("<FONT FACE=symbol>\241\225</FONT>","&gt;&lt;", "&gt;&lt;");;

let leftarrow = ("<FONT FACE=symbol>\172</FONT>","&lt;-", "&lt;-");;
let upleftarrow = ("<FONT FACE=symbol>\229</FONT>","&lt;=", "&lt;=");;
let rightarrow = ("<FONT FACE=symbol>\174</FONT>","-&gt;", "-&gt;");;
let uprightarrow = ("<FONT FACE=symbol>\222</FONT>","=&gt;", "=&gt;");;
let leftrightarrow = ("<FONT FACE=symbol>\171</FONT>","&lt;-&gt;", "&lt;-&gt;");;
let upleftrightarrow = ("<FONT FACE=symbol>\219</FONT>","&lt;=&gt;", "&lt;=&gt;");;

let infty = ("<FONT FACE=symbol>\165</FONT>","oo", "oo");;
let int = ("<FONT FACE=symbol>\242</FONT>","int", "integrale");;
let display_int = ("<FONT FACE=symbol>\243<BR>\245</FONT>",
"/<BR>|<BR>/", "/<BR>|<BR>/");;

let exists = ("<FONT FACE=symbol>\036</FONT>","exists", "il existe");;
let forall = ("<FONT FACE=symbol>\034</FONT>","for all", "pour tout");;

let notin  = ("<FONT FACE=symbol>\207</FONT>","not in", "n'appartient pas à");;

let lfloor = ("<FONT FACE=symbol>\235</FONT>","|_", "|_");;
let rfloor = ("<FONT FACE=symbol>\251</FONT>","_|", "_|");;
let lceil = ("<FONT FACE=symbol>\233</FONT>","|", "|");;
let rceil = ("<FONT FACE=symbol>\249</FONT>","|", "|");;
let langle = ("<FONT FACE=symbol>\225</FONT>","&lt;", "&lt;");;
let rangle = ("<FONT FACE=symbol>\241</FONT>","&gt;", "&gt;");;


let tr = function
  "<" -> "&lt;"
| ">" -> "&gt;"
| "\\{" -> "{"
| "\\}" -> "}"
| s   -> s
;;

let put_delim  skip put d n =
  let rec do_rec s i =
    if i >= 1 then begin
      put s;
      skip () ;
      do_rec s (i-1)
    end in
  if !now <> Symbol || n=1 then
    let d = tr d in
    do_rec d n
  else begin
    put "<FONT FACE=symbol>\n" ;
    if d = "(" then begin
      put "&#230;" ; skip () ;
      do_rec "&#231" (n-2) ;
      put "&#232;"
    end else if d=")" then begin
      put "&#246;" ; skip () ;
      do_rec "&#247" (n-2) ;
      put "&#248;"
    end else if d = "[" then begin
      put "&#233;" ; skip () ;
      do_rec "&#234" (n-2) ;
      put "&#235;"
    end else if d="]" then begin
      put "&#249;" ; skip () ;
      do_rec "&#250" (n-2) ;
      put "&#251;"
   end else if d = "\\lfloor" then begin
      do_rec "&#234" (n-1) ;
      put "&#235;"
    end else if d="\\rfloor" then begin
      do_rec "&#250" (n-1) ;
      put "&#251;"
    end else if d = "\\lceil" then begin
      put "&#233;" ; skip () ;
      do_rec "&#234" (n-1)
    end else if d="\\rceil" then begin
      put "&#249;" ; skip () ;
      do_rec "&#250" (n-1)
    end else if d="|" then begin
      do_rec "&#189;" n
    end else if d="\\|" then begin
      do_rec "&#231;&#231;" n
    end else if d = "\\{" then begin
      put "&#236;" ; skip () ;
      do_rec "&#239" ((n-3)/2) ;
      put "&#237;" ; skip () ;
      do_rec "&#239" ((n-3)/2) ;
      put "&#238;"     
    end else if d = "\\}" then begin
      put "&#252;" ; skip () ;
      do_rec "&#239" ((n-3)/2) ;
      put "&#253;" ; skip () ;
      do_rec "&#239" ((n-3)/2) ;
      put "&#254;"     
    end ;
    put "</FONT FACE=symbol>\n"
  end
;;


   
  

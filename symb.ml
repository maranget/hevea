type t = Symbol | Francais | English
;;

let now = ref Francais
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

let star = ("star","star","étoile");;
let cap = ("<FONT FACE=symbol>\199</FONT>","inter", "inter");;
let cup = ("<FONT FACE=symbol>\200</FONT>","U", "U");;
let vee = ("<FONT FACE=symbol>\218</FONT>","\\/", "\\/");;
let wedge = ("<FONT FACE=symbol>\217</FONT>","/\\", "/\\");;
let infty = ("<FONT FACE=symbol>\165</FONT>","oo", "oo");;


let exists = ("<FONT FACE=symbol>\036</FONT>","oo", "oo");;
let forall = ("<FONT FACE=symbol>\034</FONT>","oo", "oo");;

let tr = function
  "<" -> "&lt;"
| ">" -> "&gt;"
| "\\{" -> "{"
| "\\}" -> "}"
| s   -> s
;;

let put_delim skip put d n =
  let rec do_rec s i =
    if i >= 1 then begin
      put s;
      skip () ;
      do_rec s (i-1)
    end in
  let d = tr d in
  if !now <> Symbol || n=1 then
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
    end else if d = "{" then begin
      put "&#236;" ; skip () ;
      do_rec "&#239" ((n-3)/2) ;
      put "&#237;" ; skip () ;
      do_rec "&#239" ((n-3)/2) ;
      put "&#238;"     
    end else if d = "}" then begin
      put "&#252;" ; skip () ;
      do_rec "&#239" ((n-3)/2) ;
      put "&#253;" ; skip () ;
      do_rec "&#239" ((n-3)/2) ;
      put "&#254;"     
    end ;
    put "</FONT FACE=symbol>\n"
  end
;;


   
  

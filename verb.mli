exception VError of string

module type S = sig val init : unit -> unit end


module MakeAlso
    (Dest : OutManager.S) (Image : ImageManager.S)
    (Scan : Latexscan.S) (Lexget : Lexget.S) : S


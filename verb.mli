exception Error of string

module type S = sig val init : unit -> unit end


module MakeAlso (Dest : OutManager.S) (Scan : Latexscan.S) : S


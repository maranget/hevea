module type S =
 functor (Dest : OutManager.S) ->
   functor (Image : ImageManager.S) ->
     functor (Scan : Latexscan.S) ->
       sig end


module
    Make
    (Dest: OutManager.S) (Image : ImageManager.S) (Scan : Latexscan.S)
    (ToMake : S) =
struct
  module Rien = ToMake (Dest) (Image) (Scan)
  
end

let value s = match String.uppercase s with
  "DOCUMENT"|"" -> 0
| "PART" -> 1
| "CHAPTER" -> 2
| "SECTION" -> 3
| "SUBSECTION" -> 4
| "SUBSUBSECTION" -> 5
| _         -> 6
;;

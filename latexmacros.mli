type env =
  Style of string
| Font of int
| Color of string

val pretty_env : env -> string


type action =
    Print of string
  | Open of (string * string)
  | Close of string
  | Print_arg of int
  | Print_fun of ((string -> string) * int)
  | Save_arg of int
  | Print_saved
  | Subst of string
  | New_count of int
  | Set_count of (int * int)
  | Add_count of (int * int)
  | Print_count of ((int -> string)  * int)
  | Env of env
  | Test of bool ref
  | SetTest of (bool ref * bool)
  | IfCond of bool ref * action list * action list
;;

type pat =  string list * string list
;;

val pretty_pat : pat -> unit

val verbose: int ref

val find_macro: string -> pat * action list

val def_macro_pat: string -> pat  -> action list -> unit
val def_macro: string -> int -> action list -> unit
val def_env: string -> action list -> action list -> unit
val newif : string -> unit

val reg : string ref

val display :  bool ref

val invisible : string -> bool

val limit : string -> bool


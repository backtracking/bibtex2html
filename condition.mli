

type constante =
(*
  | Key
*)
  | Field of string

;;
   
type condition =
  | True 
  | False 
(*
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | Comp of constante * string * constante
*)
  | Match of constante * Str.regexp
;;


val evaluate_cond : (string * Bibtex.atom list) list -> condition -> bool;;



open Condition;;

exception Syntax_error;;

let condition s =
  try
    let n = String.index s ':' in 
    let field = String.uppercase (String.sub s 0 n)
    and filter = String.sub s (succ n) (String.length s - n - 1)
    in
      Printf.printf "champ = %s\n" field;
      Printf.printf "filter = %s\n" filter;
      Match (Field field,Str.regexp_case_fold filter)
  with 
      Not_found ->
	raise Syntax_error
;;


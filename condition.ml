

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

let evaluate_cond fields = function
  | True -> true
  | False -> false
  | Match(Field(f),r) ->
      try
	match List.assoc f fields with
	  | [Bibtex.String(v)] -> 
	      begin
		try 
		  let _ = Str.search_forward r v 0 in true
		with
		    Not_found -> false
	      end

	  | [Bibtex.Id(v)] -> 
	      begin
		try 
		  let _ = Str.search_forward r v 0 in true
		with
		    Not_found -> false
	      end


	  | _ -> false
      with
	  Not_found -> false
;;

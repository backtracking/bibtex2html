(*
 * latexmacros.mli
 *
 * Code initially written by Xavier Leroy.
 *)

type action =
    Print of string
  | Print_arg
  | Skip_arg
  | Raw_arg of (string -> unit)

val def : string -> action list -> unit

val find_macro: string -> action list

val out_channel : out_channel ref
val print_s : string -> unit
val print_c : char -> unit

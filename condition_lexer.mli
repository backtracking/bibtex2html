
exception Lex_error of string;;


val token : Lexing.lexbuf -> Condition_parser.token;;

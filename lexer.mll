{
open Parser
exception Eof
exception Syntax_error of string
}
rule token = parse
    [' ' '\t' '\n'] { token lexbuf }
  | ['0'-'9']+ as i { INT(int_of_string i) }
  | [^'(' ')' ' ']+ as s    { SYM(s) }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | eof             { EOF }
  | _ { raise (Syntax_error("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

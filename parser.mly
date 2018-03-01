%{
open Sexp;;
%}
%token <int> INT
%token <string> SYM
%token LPAREN RPAREN
%token EOF

%start main             /* the entry point */
%type <Sexp.sexp> main
%%
main:
    expr EOF   { $1 }
;
expr:
    INT                     { Sexp.INT $1 }
  | SYM                     { Sexp.SYM $1 }
  | LPAREN expr_list RPAREN { Sexp.SEXP $2 }
;
expr_list:
  | expr_list_rev { List.rev $1 }
;
expr_list_rev:
    expr               { $1 :: [] }
  | expr_list_rev expr { $2 :: $1 }
;

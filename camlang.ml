(****
  CAMLANG - A language.
*****)

exception Syntax_error of string
exception Unbound_identifier of string
exception Not_implemented
exception Runtime_error of string
exception Type_error

let sexp_of_string str =
  try
    Lexing.from_string str |> Parser.main Lexer.token
  with Lexer.Syntax_error(s) -> raise (Syntax_error s)

(* AST Types *)
type variable = string
type mlang =
  | Int of int
  | Bool of bool
  | Var of variable
  | Bind of variable * mlang * mlang
  | Fun of variable list * mlang
  | Call of mlang * (mlang list)

let rec arg_list = let open Sexp in function
  | [] -> []
  | SYM(x)::xs -> x :: arg_list xs
  | _ -> raise (Syntax_error "Non-variable in function arg list.")
(* Parses s-expressions into an MLang AST *)
let rec parse = let open Sexp in function
  | SEXP[SYM("bind");SEXP[SYM(id);bound_expr]; body] ->
    Bind (id, parse bound_expr, parse body)
  | SEXP[SYM("fun");SEXP(ids); body] ->
    Fun (arg_list ids, parse body)
  | SEXP(x::xs) -> Call(parse x, List.map parse xs)
  | INT x -> Int x
  | SYM x -> Var x
  | _ -> raise (Syntax_error "Bad syntax")


(* Runtime Types *)
type mlang_env = (variable * mlang_val) list
and mlang_val =
  | IntV of int
  | BoolV of bool
  | FunV of variable list * mlang * mlang_env
  | Primitive of (mlang_val list -> mlang_val)

(* get the value of the given identifier in the given environment.
   Raises Unbound_identifier if the identifier is not bound to a value. *)
let rec lookup env name =
  match env with
  | [] -> raise (Unbound_identifier name)
  | (n,v)::xs -> if n = name then v else lookup xs name

let extend env name value = (name,value)::env

let rec eval env lang =
  let lookup = lookup env in
  match lang with
  | Int i -> IntV i
  | Bool b -> BoolV b
  | Var x -> lookup x
  | Fun (v,l) -> FunV (v,l,env)
  | Bind (v,b,l) -> eval (extend env v (eval env b)) l
  | Call (f,xs) ->
      let args = List.map (eval env) xs in
      (match eval env f with
        | FunV (a,body,e) ->
            let fun_env = List.combine a args in
            eval (List.rev_append fun_env e) body
        | Primitive p -> p args
        | _ -> raise Type_error)

let default_env =
  let int_op op = function
     | IntV(x1)::IntV(x2)::xs -> IntV(op x1 x2)
     | _ -> raise (Runtime_error "Incorrect arguments provided") in
  [
    "+", Primitive (int_op (+));
    "-", Primitive (int_op (-));
    "*", Primitive (int_op ( * ));
  ]

let string_of_mlang_val = function
  | IntV i -> Printf.sprintf "%d" i
  | BoolV b -> Printf.sprintf "%B" b
  | FunV _ | Primitive _ -> Printf.sprintf "#<procedure>"

let run code env = sexp_of_string code |> parse |> eval env

let _ =
  Printf.printf "CAMLANG Toplevel\n";
  try while true; do
    Printf.printf "> %!";
    let str = input_line stdin in
    try
      run str default_env
      |> string_of_mlang_val
      |> Printf.printf "%s\n"
    with
      | Syntax_error(s) -> Printf.printf "Syntax error: %s\n" s
      | Unbound_identifier(s) -> Printf.printf "Error: Unbound identifier %s\n" s
      | Runtime_error(s) -> Printf.printf "Error: %s\n" s
      | Type_error -> Printf.printf "Error: Unexpected type\n"
      | _ -> Printf.printf "Error: could not run input expression\n"
  done; ()
  with End_of_file -> ()

open Core.Std
open Lexer
open Lexing

open Ast

let print_position outc lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outc "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Some (Parser.prog Lexer.read lexbuf) with
  | SyntaxError msg ->
    fprintf stdout "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stdout "%a: syntax error\n" print_position lexbuf;
    None

let print_expr = function
  | Num n ->
    print_string "Num(";
    print_string (string_of_int n);
    print_string ")"
  | Var v ->
    print_string "Var(";
    print_string v;
    print_string ")"

let print_statement = function
  | Assign (s, expr) ->
    print_string "Assign(";
    print_string s;
    print_string ",";
    print_expr expr;
    print_string ")"
  | Print expr ->
    print_string "Print(";
    print_expr expr;
    print_string ")"

let print_ast = function
  | Program statements -> 
    print_string "Program([";
    List.iteri ~f:(fun i v ->
      if i > 0 then print_string ", ";
      print_statement v) statements;
    print_string "])\n"

let () =
  let contents = In_channel.read_all "test.edl" in
  let lexbuf = Lexing.from_string contents in
  begin match parse_with_error lexbuf with
    | Some p -> print_ast(p)
    | None -> print_endline("failure")
  end

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

let print_tree_fmt o i =
  print_string o; print_string "("; print_string i; print_string ")"

let print_val = function
  | Value.Num n -> print_tree_fmt "Num" (string_of_int n)
  | Value.String s -> print_tree_fmt "Str" s
  | Value.None -> print_tree_fmt "None" ""

let rec print_expr = function
  | Expression.Val v -> print_tree_fmt "Val" (print_val v; "")
  | Expression.Var v -> print_tree_fmt "Var" v
  | Expression.Funccall (name, params) -> 
    print_string "Func("; print_string name; print_string ",";
    List.iter params ~f:(fun p -> print_expr p; print_string ","); print_string ")"

let rec print_statement = function
  | Statement.If (expr, statements) -> 
    print_string "If("; print_expr expr; print_string ",["; 
    List.iter statements ~f:(fun s -> print_statement s; print_string ","); print_string "])"
  | Statement.Ifelse (expr, ss1, ss2) ->
    print_string "Ifelse("; print_expr expr; print_string ",[";
    List.iter ss1 ~f:(fun s -> print_statement s); print_string "],[";
    List.iter ss2 ~f:(fun s -> print_statement s); print_string "])"
  | Statement.Assign (s, expr) -> 
    print_string "Assign("; print_string s; print_string ","; print_expr expr; print_string ")"
  | Statement.Funccall (s, es) ->
    print_string "Funccall("; print_string s; print_string ",[";
    List.iter es ~f:(fun e -> print_expr e; print_string ","); print_string "])"
  | Statement.Return e ->
    print_string "Return("; print_expr e; print_string ")"

let print_funcdef {Funcdef.name = n; Funcdef.params = ps; Funcdef.statements = ss} =
  print_string "Funcdef("; print_string n; print_string ",[";
  List.iter ps ~f:(fun p -> print_string p; print_string ","); print_string "],";
  List.iter ss ~f:(fun s -> print_statement s; print_string ","); print_string "])"

let print_program { Program.main = m; Program.funcs = fs } =
  print_string "Program([";
  List.iter fs ~f:(fun f -> print_funcdef f); print_string "],";
  List.iter m ~f:(fun s -> print_statement s); print_string ")"

let () =
  let contents = In_channel.read_all "test.edl" in
  let lexbuf = Lexing.from_string contents in
  begin match parse_with_error lexbuf with
    | Some p -> print_program(p)
    | None -> print_endline("failure")
  end

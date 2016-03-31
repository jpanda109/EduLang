open Core.Std
open Lexing

open Lexer
open Ast

exception Unbound_variable of string
exception Unbound_function of string
exception Unimplemented of string

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

let eval_expr ctx = function
  | Expression.Val v -> v
  | Expression.Var s -> 
    begin match Map.find ctx s with
    | None -> raise (Unbound_variable s)
    | Some x -> x
    end
  | Expression.Funccall (name, params) -> raise (Unbound_function name)

let eval_statement ctx = function
  | Statement.Assign (s, expr) ->
    Map.add ctx ~key:s ~data:(eval_expr ctx expr)
  | Statement.Print expr -> 
    begin match eval_expr ctx expr with
    | Value.Num n -> print_endline (string_of_int n)
    | Value.String s -> print_endline s
    end; ctx
  | Statement.If (expr, statements) ->
    raise (Unimplemented "If")
  | Statement.Ifelse (expr, if_st, else_st) ->
    raise (Unimplemented "Ifelse")
  | Statement.Funccall (name, exprs) ->
    raise (Unimplemented "Funccall")

let rec eval_prog ctx = function
  | hd::tl -> eval_prog (eval_statement ctx hd) tl
  | [] -> ()

let eval_ast ctx prog =
  eval_prog ctx prog.Program.main

let () =
  let contents = In_channel.read_all "test.edl" in
  let lexbuf = Lexing.from_string contents in
  begin match parse_with_error lexbuf with
    | Some ast -> eval_ast String.Map.empty ast
    | None -> print_endline("failure")
  end

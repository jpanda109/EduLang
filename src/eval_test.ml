open Core.Std
open Lexing

open Lexer
open Ast

exception Unbound_variable of string
exception Unbound_function of string

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
  | Val v -> v
  | Var s -> 
    begin match Map.find ctx s with
    | None -> raise (Unbound_variable s)
    | Some x -> x
    end
  | Funccall (name, params) -> raise (Unbound_function name)

let eval_statement ctx = function
  | Assign (s, expr) ->
    Map.add ctx ~key:s ~data:(eval_expr ctx expr)
  | Print expr -> 
    begin match eval_expr ctx expr with
    | Num n -> print_endline (string_of_int n)
    | String s -> print_endline s
    end; ctx

let rec eval_prog ctx = function
  | hd::tl -> eval_prog (eval_statement ctx hd) tl
  | [] -> ()

let eval_ast ctx prog =
  eval_prog ctx prog.main

let () =
  let contents = In_channel.read_all "test.edl" in
  let lexbuf = Lexing.from_string contents in
  begin match parse_with_error lexbuf with
    | Some ast -> eval_ast String.Map.empty ast
    | None -> print_endline("failure")
  end

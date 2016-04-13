open Core.Std
open Lexing

open Lexer

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

let get_ast filename =
  let contents = In_channel.read_all filename in
  let lexbuf = Lexing.from_string contents in
  begin match parse_with_error lexbuf with
  | Some ast -> ast
  | None -> raise (Failure "couldn't parse file")
  end


let eval_file filename =
  get_ast filename |> Eval.eval_ast

let transpile_file filename = get_ast filename |> Transpile.eval_prog stdout

let eval =
  Command.basic
    ~summary:"evaluate an edl language file"
    Command.Spec.(
      empty
        +> anon ("filename" %: file)
    )
    (fun filename () -> eval_file filename)

let transpile =
  Command.basic
    ~summary:"transpile an edl language file to javascript"
    Command.Spec.(
      empty
        +> anon ("filename" %: file)
    )
    (fun filename () -> transpile_file filename)

let command =
  Command.group ~summary:"stuff"
    [ "eval", eval;
      "transpile", transpile ]

let () =
  Command.run command

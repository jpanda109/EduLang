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

let eval_file filename =
  let contents = In_channel.read_all filename in
  let lexbuf = Lexing.from_string contents in
  begin match parse_with_error lexbuf with
  | Some ast -> Eval.eval_ast ast
  | None -> print_endline("failure")
  end

let spec =
  let open Command.Spec in
  empty
    +> anon ("filename" %: file)

let command =
  Command.basic
    ~summary:"evaluate an edl language file"
    ~readme:(fun () -> "todo: readme")
    spec
    (fun filename () -> eval_file filename)

let () =
  Command.run command

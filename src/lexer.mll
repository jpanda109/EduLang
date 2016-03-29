{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let main = "main"
let print = "print"
let num = ['0'-'9']+
let var = ['a'-'z' 'A'-'Z' '_' '-']+
let assign = ":="
let semicolon = ';'
let lparen = '('
let rparen = ')'
let lbrace = '{'
let rbrace = '}'

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | main { MAIN }
  | print { PRINT }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | assign { ASSIGN }
  | semicolon { SEMICOLON }
  | lparen { LPAREN }
  | rparen { RPAREN }
  | lbrace { LBRACE }
  | rbrace { RBRACE }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

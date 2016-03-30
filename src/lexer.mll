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
let funcdef = "fun"
let print = "print"
let num = ['0'-'9']+
let id = ['a'-'z' 'A'-'Z' '_' '-']+
let str = '"'_*'"'
let assign = ":="
let semicolon = ';'
let comma = ','
let lparen = '('
let rparen = ')'
let lbrace = '{'
let rbrace = '}'

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | main { MAIN }
  | funcdef { FUNCDEF }
  | print { PRINT }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  | str { 
      let s = Lexing.lexeme lexbuf in
      STR (String.sub s 1 ((String.length s) - 2))
    }
  | assign { ASSIGN }
  | semicolon { SEMICOLON }
  | comma { COMMA }
  | lparen { LPAREN }
  | rparen { RPAREN }
  | lbrace { LBRACE }
  | rbrace { RBRACE }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

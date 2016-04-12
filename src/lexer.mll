{
open Lexing
open Parser
exception SyntaxError of string
}


rule read =
  parse
  | [' ' '\t']+ { read lexbuf }
  | '\r' | '\n' | "\r\n" { new_line lexbuf; read lexbuf }
  | "main" { MAIN }
  | "func" { FUNCDEF }
  | "return" { RETURN }
  | "true" { BOOL (Lexing.lexeme lexbuf) }
  | "false" { BOOL (Lexing.lexeme lexbuf) }
  | "if" { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  | "for" { FOR }
  | "in" { IN }
  | "to" { TO }
  | "import" { IMPORT }
  | "as" { AS }
  | ['0'-'9']+'.'?['0'-'9']* { NUM (Lexing.lexeme lexbuf) }
  | ['a'-'z' 'A'-'Z' '_' '-']+ { ID (Lexing.lexeme lexbuf) }
  | ":=" { ASSIGN }
  | ';' { SEMICOLON }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }
  | '/' { DIV }
  | "==" { EQUALITY }
  | "!=" { INEQUALITY }
  | "<=" { LTEQ }
  | ">=" { GTEQ }
  | "<" { LT }
  | ">" {GT}
  | '"' { read_string (Buffer.create 17) lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
and read_string buf =
  parse
  | '"' { STR (Buffer.contents buf) }
  | '\\' '/' { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b' { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f' { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

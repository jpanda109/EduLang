%{

  open Ast

%}
%token <int> NUM
%token MAIN
%token PRINT
%token <string> VAR
%token <string> STR
%token ASSIGN
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token SEMICOLON
%token EOF

%start <Ast.ast> prog

%%
prog:
  MAIN; LBRACE; ss = statements; RBRACE; EOF; { Program ss } ;

statements:
  s_list = list(statement)            { s_list } ;

statement:
  | v = VAR; ASSIGN; e = expression; SEMICOLON { Assign (v, e) }
  | PRINT; LPAREN; e = expression; RPAREN; SEMICOLON             { Print e }
  ;

expression:
  | n = NUM { Val (Num n) }
  | v = VAR { Var v }
  | s = STR { Val (String s) }
  ;


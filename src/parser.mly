%{

  open Ast

%}
%token <int> NUM
%token MAIN
%token FUNCDEF
%token PRINT
%token <string> ID
%token <string> STR
%token ASSIGN
%token SEMICOLON
%token COMMA
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token EOF

%start <Ast.program> prog

%%
prog:
  defs1 = list(funcdef) ;MAIN; LBRACE; ss = statements; RBRACE; defs2 = list(funcdef); EOF; 
    { { main = ss; funcs = defs1 @ defs2 } } ;

funcdef:
  FUNCDEF; LPAREN; params = separated_list(COMMA, ID); RPAREN; RBRACE; ss = statements; LBRACE
    { {params = params; statements = ss} } ;

statements:
  s_list = list(statement)            { s_list } ;

statement:
  | v = ID; ASSIGN; e = expression; SEMICOLON { Assign (v, e) }
  | PRINT; LPAREN; e = expression; RPAREN; SEMICOLON             { Print e }
  ;

expression:
  | name = ID; LPAREN; params = separated_list(COMMA, expression); RPAREN 
    { Funccall (name, params) }
  | n = NUM { Val (Num n) }
  | v = ID { Var v }
  | s = STR { Val (String s) }
  ;


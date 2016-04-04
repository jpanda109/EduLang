%{

  open Ast

%}
%token <int> NUM
%token MAIN
%token FUNCDEF
%token IF
%token ELSE
%token <string> ID
%token <string> STR
%token ASSIGN
%token RETURN
%token SEMICOLON
%token COMMA
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token EOF

%start <Ast.Program.t> prog

%%
prog:
  defs1 = list(funcdef); MAIN; LBRACE; ss = statements; RBRACE; defs2 = list(funcdef); EOF; 
    { { main = ss; funcs = defs1 @ defs2 } } ;

funcdef:
  FUNCDEF; name = ID; LPAREN; params = separated_list(COMMA, ID); RPAREN; LBRACE; ss = statements; RBRACE
    { {name = name; params = params; statements = ss} } ;

statements:
  | s_list = list(statement); rstat = returnstat { s_list@[rstat] }
  | s_list = list(statement)            { s_list } ;

statement:
  | v = ID; ASSIGN; e = expression; SEMICOLON { Statement.Assign (v, e) }
  | IF; e1 = expression; LBRACE; s1 = statements; RBRACE; ELSE; LBRACE; s2 = statements; RBRACE 
    { Ifelse (e1, s1, s2) }
  | IF; e = expression; LBRACE; ss = statements; RBRACE { Statement.If (e, ss) }
  | name = ID; LPAREN; params = separated_list(COMMA, expression); RPAREN; SEMICOLON
    { Statement.Funccall {name = name; params = params} }
  ;

returnstat:
  RETURN; e = expression; SEMICOLON { Statement.Return e }

expression:
  | name = ID; LPAREN; params = separated_list(COMMA, expression); RPAREN 
    { Expression.Funccall {name = name; params = params} }
  | n = NUM { Expression.Val (Num n) }
  | s = STR { Expression.Val (String s) }
  | v = ID { Expression.Var v }
  ;


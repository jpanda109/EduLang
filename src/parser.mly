%{

  open Ast

%}
%token <string> NUM
%token MAIN
%token FUNCDEF
%token RETURN
%token IF
%token ELSE
%token <string> ID
%token <string> STR
%token ASSIGN
%token <string> BOOL
%token SEMICOLON
%token COMMA
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token PLUS
%token MINUS
%token MULT
%token DIV
%token EQUALITY
%token INEQUALITY
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
  | s_list = list(statement); rstat = returnstat            { s_list@[rstat] } ;
  | s_list = list(statement)            { s_list } ;

statement:
  | v = ID; ASSIGN; e = expression; SEMICOLON { Statement.Assign (v, e) }
  | IF; e1 = expression; LBRACE; ss = statements; RBRACE; es = option(else_block)
    { Ifelse (e1, ss, es) }
  | name = ID; LPAREN; params = separated_list(COMMA, expression); RPAREN; SEMICOLON
    { Statement.Funccall {name = name; params = params} }
  ;

else_block:
  ELSE; LBRACE; ss = statements; RBRACE { ss };

returnstat:
  RETURN; e = expression; SEMICOLON { Statement.Return e } ;

expression:
  | name = ID; LPAREN; params = separated_list(COMMA, expression); RPAREN 
    { Expression.Funccall {name = name; params = params} }
  | e1 = expression; PLUS; e2 = expression 
    { Expression.Plus (e1, e2) }
  | e1 = expression; MINUS; e2 = expression 
    { Expression.Minus (e1, e2) }
  | e1 = expression; MULT; e2 = expression 
    { Expression.Mult (e1, e2) }
  | e1 = expression; DIV; e2 = expression 
    { Expression.Div (e1, e2) }
  | e1 = expression; EQUALITY; e2 = expression
    { Expression.Equality (e1, e2) }
  | e1 = expression; INEQUALITY; e2 = expression
    { Expression.Inequality (e1, e2) }
  | n = NUM { Expression.Val (Num (Number.number_of_string n)) }
  | s = STR { Expression.Val (String s) }
  | v = ID { Expression.Var v }
  | b = BOOL { Expression.Val (Bool (bool_of_string b)) }
  ;


%{

  open Ast

%}
%token MAIN FUNCDEF RETURN IF ELSE WHILE FOR IN TO EOF
%token <string> NUM
%token <string> ID
%token <string> STR
%token ASSIGN
%token <string> BOOL
%token SEMICOLON
%token COMMA
%token LBRACE RBRACE LBRACK RBRACK
%token LPAREN RPAREN
%token PLUS MINUS MULT DIV
%token EQUALITY INEQUALITY LTEQ GTEQ LT GT

%nonassoc EQUALITY INEQUALITY LTEQ GTEQ LT GT
%left PLUS MINUS
%left MULT DIV

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
  | v = ID; ASSIGN; e = expr; SEMICOLON { Statement.Assign (v, e) }
  | IF; e1 = expr; LBRACE; ss = statements; RBRACE; es = option(else_block)
    { Ifelse (e1, ss, es) }
  | WHILE; e = expr; LBRACE; ss = statements; RBRACE
    { While (e, ss) }
  | FOR; v = ID; IN; e1 = expr; TO; e2 = expr; LBRACE; ss = statements; RBRACE
    { For (v, e1, e2, ss) }
  | name = ID; LPAREN; params = separated_list(COMMA, expr); RPAREN; SEMICOLON
    { Statement.Funccall {name = name; params = params} }
  ;

else_block:
  ELSE; LBRACE; ss = statements; RBRACE { ss };

returnstat:
  RETURN; e = expr; SEMICOLON { Statement.Return e } ;

expr:
  | name = ID; LPAREN; params = separated_list(COMMA, expr); RPAREN 
    { Expr.Funccall {name = name; params = params} }
  | e1 = expr; PLUS; e2 = expr 
    { Expr.Plus (e1, e2) }
  | e1 = expr; MINUS; e2 = expr 
    { Expr.Minus (e1, e2) }
  | e1 = expr; MULT; e2 = expr 
    { Expr.Mult (e1, e2) }
  | e1 = expr; DIV; e2 = expr 
    { Expr.Div (e1, e2) }
  | e1 = expr; EQUALITY; e2 = expr
    { Expr.Equality (e1, e2) }
  | e1 = expr; INEQUALITY; e2 = expr
    { Expr.Inequality (e1, e2) }
  | e1 = expr; LTEQ; e2 = expr
    { Expr.LTEQ (e1, e2) }
  | e1 = expr; GTEQ; e2 = expr
    { Expr.GTEQ (e1, e2) }
  | e1 = expr; LT; e2 = expr
    { Expr.LT(e1, e2) }
  | e1 = expr; GT; e2 = expr
    { Expr.GT (e1, e2) }
  | n = NUM { Expr.Val (Num (Number.number_of_string n)) }
  | s = STR { Expr.Val (String s) }
  | v = ID { Expr.Var v }
  | b = BOOL { Expr.Val (Bool (bool_of_string b)) }
  | LBRACK; es = separated_list(COMMA, expr); RBRACK { ListInit es }
  ;


type program = 
  { main: statement list;
    funcs: funcdef list
  }

and funcdef =
  { params: string list;
    statements: statement list
  }

and statement =
  | Print of expr
  | Assign of string * expr

and expr =
  | Val of value
  | Var of string
  | Funccall of string * expr list

and value =
  | Num of int
  | String of string

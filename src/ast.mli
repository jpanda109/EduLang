type ast =
  | Program of ast list
  | Assign of string * ast
  | Print of ast
  | Num of int
  | Var of string

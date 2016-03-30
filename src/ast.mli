type ast =
  | Program of statement list

and statement =
  | Print of expr
  | Assign of string * expr

and expr =
  | Val of value
  | Var of string

and value =
  | Num of int
  | String of string

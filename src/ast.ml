module rec Value : sig
  type t =
    | Num of Number.t
    | String of string
    | Bool of bool
    | None
end = Value

and Expression : sig 
  type t =
    | Val of Value.t
    | Var of string
    | Funccall of Funccall.t
    | Plus of t * t
    | Minus of t * t
    | Mult of t * t
    | Div of t * t
    | Equality of t * t
    | Inequality of t * t
end = Expression

and Statement : sig
  type t =
    | Ifelse of Expression.t * t list * t list option
    | Assign of string * Expression.t
    | Funccall of Funccall.t
    | Return of Expression.t
end = Statement

and Funccall : sig
  type t =
    { name: string;
      params: Expression.t list
    }
end = Funccall

and Funcdef : sig
  type t =
    { name: string;
      params: string list;
      statements: Statement.t list
    }
end = Funcdef

and Program : sig
  type t =
    { main: Statement.t list;
      funcs: Funcdef.t list
    }
end = Program



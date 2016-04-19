module rec Value : sig
  type t =
    | Num of Number.t
    | String of string
    | Bool of bool
    | List of t list
    | None
end = Value

and Expr : sig 
  type t =
    | Val of Value.t
    | Var of VarCall.t
    | Funccall of Funccall.t
    | Plus of t * t
    | Minus of t * t
    | Mult of t * t
    | Div of t * t
    | Equality of t * t
    | Inequality of t * t
    | LTEQ of t * t
    | GTEQ of t * t
    | LT of t * t
    | GT of t * t
    | ListInit of t list
end = Expr

and Statement : sig
  type t =
    | Ifelse of Expr.t * t list * t list option
    | While of Expr.t * t list
    | For of string * Expr.t * Expr.t * t list
    | Assign of VarCall.t * Expr.t
    | Funccall of Funccall.t
    | ListInit of Expr.t list
    | Return of Expr.t
end = Statement

and VarCall : sig
  type t =
    | Reg of string
    | List of string * Expr.t
end = VarCall

and Funccall : sig
  type t =
    { name: string;
      params: Expr.t list
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


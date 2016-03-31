module Value = struct
  type t =
    | Num of int
    | String of string
end

module Expression = struct
  type t =
    | Val of Value.t
    | Var of string
    | Funccall of string * t list
end

module Statement = struct
  type t =
    | Print of Expression.t
    | If of Expression.t * t list
    | Ifelse of Expression.t * t list * t list
    | Assign of string * Expression.t
    | Funccall of string * Expression.t list
end

module Funcdef = struct
  type t =
    { params: string list;
      statements: Statement.t list
    }
end

module Program = struct
  type t =
    { main: Statement.t list;
      funcs: Funcdef.t list
    }
end


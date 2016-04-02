module Value = struct
  type t =
    | Num of Number.t
    | String of string
    | None
end

module Expression = struct
  type t =
    | Val of Value.t
    | Var of string
    | Funccall of string * t list
end

module Statement = struct
  type t =
    | If of Expression.t * t list
    | Ifelse of Expression.t * t list * t list
    | Assign of string * Expression.t
    | Funccall of string * Expression.t list
    | Return of Expression.t
end

module Funcdef = struct
  type t =
    { name: string;
      params: string list;
      statements: Statement.t list
    }
end

module Program = struct
  type t =
    { main: Statement.t list;
      funcs: Funcdef.t list
    }
end



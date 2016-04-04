open Core.Std

type t

val number_of_string : string -> t

val string_of_number : t -> string

val add : t -> t -> t

val sub : t -> t -> t

val mult : t -> t -> t

val div : t -> t -> t

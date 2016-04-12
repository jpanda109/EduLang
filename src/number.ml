open Core.Std

type t = float

let number_of_string = Float.of_string

let string_of_number = Float.to_string

let add a b = a +. b

let sub a b = a -. b

let mult a b = a *. b

let div a b = a /. b

let lteq = (<=)

let gteq = (>=)

let lt = (<)

let gt = (>)

let eq = (=)

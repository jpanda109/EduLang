open Core.Std

type t = int list * int list

let number_of_string s =
  let pad (w, d) =
    let padded l = List.init (10 - List.length l) ~f:(fun n -> 0) in
    ((padded w) @ w, d @ (padded d))
    in
  let rec f s i after (w, d) =
    if i = String.length s
    then 
      if List.length w > 10 || List.length d > 10
      then raise (Failure "number too long")
      else pad (w, d)
    else 
    if String.get s i = '.'
    then f s (i+1) true (w, d)
    else 
      try
        let n = String.get s i |> Char.escaped |> int_of_string in
        let new_num = if after then (w, n::d) else (n::w, d) in
        f s (i+1) after new_num
      with Failure _ -> raise (Failure "number_of_string")
  in f s 0 false ([], [])

let string_of_number (w, d) =
  let f ns = List.map ns ~f:(fun i -> string_of_int i) |> String.concat ~sep:"" in
  let truncate_zero l = List.drop_while ~f:(fun e -> e = 0) l in
  let whole = f (truncate_zero w) in
  let decimal = f (truncate_zero (List.rev d)) in
  if String.length decimal = 0
  then
    if String.length whole = 0
    then "0"
    else whole
  else String.concat ~sep:"." [whole;decimal]

let add a b = a

let sub a b = a

let mult a b = a

let div a b = a

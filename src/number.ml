type t = int list * int list

let number_of_string s =
  let rec f s i after (w, d) =
    if i = String.length s
    then (w, d)
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
  let f ns = List.map (fun i -> string_of_int i) ns |> String.concat "" in
  if List.length d = 0
  then f w
  else String.concat "." [(f w);(f d)]

let add a b = a + b

let sub a b = a - b

let mult a b = a * b

let div a b = a / b

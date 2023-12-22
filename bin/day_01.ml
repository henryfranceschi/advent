let regexp =
  Str.regexp {|[1-9]\|one\|two\|three\|four\|five\|six\|seven\|eight\|nine|}

let is_ascii_digit = function '0' .. '9' -> true | _ -> false

let parse_number s =
  if is_ascii_digit s.[0] then int_of_char s.[0] - 48
  else
    match s with
    | "zero" -> 0
    | "one" -> 1
    | "two" -> 2
    | "three" -> 3
    | "four" -> 4
    | "five" -> 5
    | "six" -> 6
    | "seven" -> 7
    | "eight" -> 8
    | "nine" -> 9
    | _ ->
        raise
          (Invalid_argument "s must be either an ascii digit or a number word")

let first s =
  let _ = Str.search_forward regexp s 0 in
  parse_number (Str.matched_string s)

let last s =
  let _ = Str.search_backward regexp s (String.length s) in
  parse_number (Str.matched_string s)

let number s = (first s * 10) + last s

let () =
  let lines = In_channel.(open_text "input/day-01" |> input_lines) in
  let sum = List.(map number lines |> fold_left ( + ) 0) in
  Printf.printf "%d\n" sum

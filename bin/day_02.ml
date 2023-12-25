type cubes = { red : int; green : int; blue : int }

let cubes_default = { red = 0; green = 0; blue = 0 }
let cubes_make red green blue = { red; green; blue }

let cubes_from_color color count =
  let count = int_of_string count in
  match color with
  | "red" -> { cubes_default with red = count }
  | "green" -> { cubes_default with green = count }
  | "blue" -> { cubes_default with blue = count }
  | _ -> raise (Invalid_argument "invalid string")

let cubes_max a b =
  cubes_make (max a.red b.red) (max a.green b.green) (max a.blue b.blue)

let cubes_any f a b = f a.red b.red || f a.green b.green || f a.blue b.blue
let cubes_power a = a.red * a.blue * a.green

type game = { number : int; subsets : cubes list }

let game_max g = List.fold_left cubes_max cubes_default g.subsets

let parse_cubes s =
  match String.split_on_char ' ' s with
  | [ count; color ] -> cubes_from_color color count
  | _ -> raise (Invalid_argument "invalid string")

let parse_cube_subsets s =
  let subsets_strings = String.(split_on_char ',' s |> List.map trim) in
  List.(map parse_cubes subsets_strings |> fold_left cubes_max cubes_default)

let parse_game s =
  match String.split_on_char ':' s with
  | [ game_string; subsets_string ] ->
      let number =
        match String.split_on_char ' ' game_string with
        | [ "Game"; number_string ] -> int_of_string number_string
        | _ -> raise (Invalid_argument "invalid string")
      in
      let subsets =
        List.map parse_cube_subsets (String.split_on_char ';' subsets_string)
      in
      { number; subsets }
  | _ -> raise (Invalid_argument "invalid string")

let p1 g =
  let max_cubes = cubes_make 12 13 14 in
  List.(
    filter (fun g -> not (cubes_any ( > ) (game_max g) max_cubes)) g
    |> fold_left (fun a g -> a + g.number) 0)

let p2 g = List.(map game_max g |> map cubes_power |> fold_left ( + ) 0)

let () =
  let lines = In_channel.(open_text "input/day-02" |> input_lines) in
  let games = List.map parse_game lines in
  Printf.printf "%d, %d\n" (p1 games) (p2 games)

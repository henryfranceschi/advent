type pos = { index : int; is_first : bool; is_last : bool }
type number = { positions : pos list; parsed : int }

let is_digit = function '0' .. '9' -> true | _ -> false
let is_symbol = function 
  | '0' .. '9' -> false 
  | '.' -> false 
  | '\n' -> false
  | _ -> true

let range start stop =
  assert (stop >= start);
  List.init (abs (stop + 1) - start) (( + ) start)

let determine_pos (first, last) index =
  { index; is_first = index = first; is_last = index = last }

let pos_list_of_pair (first, last) =
  range first last |> List.map (determine_pos (first, last))

let fold_adjacent sps i =
  match sps with
  | (first, last) :: tl when i - last = 1 -> (first, i) :: tl
  | _ -> (i, i) :: sps

let number_pairs s =
  String.to_seqi s
  |> Seq.filter (fun (_, c) -> is_digit c)
  |> Seq.fold_left (fun acc (i, _) -> fold_adjacent acc i) []

let adjacencies width { index; is_first; is_last } =
  let u n = n - width in
  let d n = n + width in
  let l = index - 1 in
  let r = index + 1 in

  let left = [ u l; l; d l ] in
  let right = [ u r; r; d r ] in
  let up_down = [ u index; d index ] in

  match (is_first, is_last) with
  | true, true -> up_down @ left @ right
  | true, false -> up_down @ left
  | false, true -> up_down @ right
  | false, false -> up_down

let number_of_pair s (first, last) =
  let positions = pos_list_of_pair (first, last) in
  let parsed = int_of_string (String.sub s first (last - first + 1)) in
  { positions; parsed }

let char_at_opt s i = try Some (String.get s i) with _ -> None

let is_adjacenct_to_symbol data width number =
  number.positions
  |> List.concat_map (adjacencies width)
  |> List.filter_map (char_at_opt data)
  |> List.exists is_symbol

let () =
  let data = In_channel.(open_text "input/day-03" |> input_all) in
  let width = String.index_from data 0 '\n' + 1 in
  number_pairs data
  |> List.map (number_of_pair data)
  |> List.filter (is_adjacenct_to_symbol data width)
  |> List.map (fun n -> n.parsed)
  |> List.fold_left ( + ) 0
  |> Printf.printf "%d\n"

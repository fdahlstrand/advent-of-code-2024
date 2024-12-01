let puzzle_input = "./input/day01/input.txt"

let whitespace = Str.regexp {|[ \t]|}

let sum = List.fold_left ( + ) 0

let tuple_of_list = function
  | [a; b] ->
      (a, b)
  | _ ->
      failwith "Each entry must contain exactly two numbers"

let tuple_of_string s =
  Str.split whitespace s
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string |> tuple_of_list

let read_puzzle_input path =
  In_channel.with_open_text path In_channel.input_lines
  |> List.map tuple_of_string |> List.split

let sort_lists (a, b) = (List.sort compare a, List.sort compare b)

let rec count m = function
  | [] ->
      0
  | h :: t ->
      if h = m then 1 + count m t else count m t

let lists = read_puzzle_input puzzle_input

let distances =
  let a, b = lists |> sort_lists in
  List.map2 (fun a' b' -> abs (a' - b')) a b

let total_distance = sum distances

let similarity_scores =
  let a, b = lists in
  List.map (fun e -> e * count e b) a

let similarity_score = sum similarity_scores

let () =
  Printf.printf "\nTotal distance (part 1): %d\nSimilarity score (part 2): %d\n"
    total_distance similarity_score

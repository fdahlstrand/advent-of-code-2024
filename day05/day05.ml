let input_path = "./input/day05/sample.txt"

let parse_rules inp =
  let parse_rule r =
    Str.split (Str.regexp {|||}) r
    |> List.map int_of_string
    |> fun lst ->
    match lst with
    | [a; b] ->
        (a, b)
    | _ ->
        failwith "Unexpected rule definition"
  in
  Str.split (Str.regexp "\n") inp |> List.map parse_rule

let parse_updates inp =
  let parse_update u = Str.split (Str.regexp ",") u |> List.map int_of_string in
  Str.split (Str.regexp "\n") inp |> List.map parse_update

let read_data path =
  let input =
    In_channel.with_open_text path In_channel.input_all
    |> Str.split (Str.regexp "\n\n")
  in
  let rules, updates =
    match input with [r; u] -> (r, u) | _ -> failwith "Unexpected input data"
  in
  (parse_rules rules, parse_updates updates)

let rules, updates = read_data input_path

let rec check_page_number rs n = function
  | h :: t ->
      if List.mem (n, h) rs then check_page_number rs n t else false
  | [] ->
      true

let rec check_update rs = function
  | h :: t ->
      check_page_number rs h t :: check_update rs t
  | _ ->
      []

let mid lst =
  let n = List.length lst in
  List.nth lst (((1 + n) / 2) - 1)

let correctly_ordered_sum =
  List.filter
    (fun us -> check_update rules us |> List.for_all (fun x -> x))
    updates
  |> List.map (fun us -> mid us)
  |> List.fold_left ( + ) 0

let () = Printf.printf "\nCorrectly ordered sum: %d\n" correctly_ordered_sum

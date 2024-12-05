let input_path = "./input/day05/input.txt"

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

let rec find_failed_rules rs n = function
  | h :: t ->
      if List.mem (n, h) rs then find_failed_rules rs n t
      else (n, h) :: find_failed_rules rs n t
  | _ ->
      []

let rec find_errors rs = function
  | h :: t ->
      find_failed_rules rs h t :: find_errors rs t
  | _ ->
      []

let swap a b lst =
  let rec swap' pre a b = function
    | h :: t ->
        if h = a then swap' (pre @ [b]) a b t
        else if h = b then swap' (pre @ [a]) a b t
        else swap' (pre @ [h]) a b t
    | [] ->
        pre
  in
  swap' [] a b lst

let rec fix_update lst = function
  | (a, b) :: t ->
      fix_update (swap a b lst) t
  | [] ->
      lst

let rec fix_all rs lst =
  match lst with
  | h :: t ->
      let e = find_errors rs h |> List.flatten in
      if List.is_empty e then h :: fix_all rs t
      else fix_all rs (fix_update h e :: t)
  | [] ->
      []

let correctly_ordered_sum =
  List.filter
    (fun us -> check_update rules us |> List.for_all (fun x -> x))
    updates
  |> List.map (fun us -> mid us)
  |> List.fold_left ( + ) 0

let incorrectly_ordered_sum =
  List.filter
    (fun us -> check_update rules us |> List.exists (fun x -> not x))
    updates
  |> fix_all rules
  |> List.map (fun us -> mid us)
  |> List.fold_left ( + ) 0

let () =
  Printf.printf "\nCorrectly ordered sum: %d\nIncorrectly ordered sum: %d\n"
    correctly_ordered_sum incorrectly_ordered_sum

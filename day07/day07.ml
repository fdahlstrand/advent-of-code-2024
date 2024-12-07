let eval lst =
  let rec eval' acc = function
    | h :: t ->
        if List.is_empty acc then eval' [h] t
        else eval' (List.map (( + ) h) acc @ List.map (( * ) h) acc) t
    | [] ->
        acc
  in
  eval' [] lst

let read_equations path =
  let parse_numbers s =
    s |> String.trim |> String.split_on_char ' ' |> List.map int_of_string
  in
  let parse_equation s =
    match s |> String.split_on_char ':' with
    | [value; numbers] ->
        (int_of_string value, parse_numbers numbers)
    | _ ->
        failwith ("Error in input: " ^ s)
  in
  In_channel.with_open_text path In_channel.input_lines
  |> List.map parse_equation

let is_eqn_possible (v, ns) = List.mem v (eval ns)

let equations = read_equations "./input/day07/input.txt"

let calibration_result =
  equations
  |> List.filter is_eqn_possible
  |> List.map fst |> List.fold_left ( + ) 0

let () = Printf.printf "\nTotal calibration result: %d\n" calibration_result

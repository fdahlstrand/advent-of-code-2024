let eval ops lst =
  let rec eval' ops acc = function
    | h :: t ->
        if List.is_empty acc then eval' ops [h] t
        else
          eval' ops
            (List.fold_left ( @ ) []
               (List.map (fun op -> List.map (op h) acc) ops) )
            t
    | [] ->
        acc
  in
  eval' ops [] lst

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

let is_eqn_possible ops (v, ns) = List.mem v (eval ops ns)

let calculate ops equations =
  equations
  |> List.filter (is_eqn_possible ops)
  |> List.map fst |> List.fold_left ( + ) 0

let ( ||^ ) a b = int_of_string (string_of_int b ^ string_of_int a)

let equations = read_equations "./input/day07/input.txt"

let calibration_result = calculate [( + ); ( * )] equations

let calibration_result_2 = calculate [( + ); ( * ); ( ||^ )] equations

let () =
  Printf.printf
    "\nTotal calibration result 1: %d\nTotal calibration result 2: %d\n"
    calibration_result calibration_result_2

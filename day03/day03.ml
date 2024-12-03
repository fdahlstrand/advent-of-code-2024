let input_path = "./input/day03/input.txt"

let mulop_regex =
  Str.regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|}

let read_memory path = In_channel.with_open_text path In_channel.input_all

let analyze_memory m =
  Str.full_split mulop_regex m
  |> List.filter (function Str.Delim _ -> true | _ -> false)
  |> List.map (function Str.Delim s -> s | _ -> failwith "Unexpected input")

let read_instr instr =
  let _ = Str.string_match mulop_regex instr 0 in
  let a = Str.matched_group 1 instr in
  let b = Str.matched_group 2 instr in
  (int_of_string a, int_of_string b)

let run prog = List.map (fun (a, b) -> a * b) prog |> List.fold_left ( + ) 0

let program = read_memory input_path |> analyze_memory |> List.map read_instr

let () = Printf.printf "\nResult: %d\n" (run program)

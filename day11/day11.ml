let split_stone s =
  let n = String.length s in
  [String.sub s 0 (n / 2); String.sub s (n / 2) (n / 2)]
  |> List.map int_of_string |> List.map string_of_int

let process_stone s =
  if s = "0" then ["1"]
  else if String.length s mod 2 = 1 then [string_of_int (int_of_string s * 2024)]
  else split_stone s

let rec process_stones_aux n stones =
  let () = Printf.printf "%d\n%!" n in
  if n = 0 then stones
  else process_stones_aux (n - 1) (List.map process_stone stones |> List.concat)

let stones =
  In_channel.with_open_text "./input/day11/input.txt" In_channel.input_all
  |> String.trim |> String.split_on_char ' '

let () =
  Printf.printf "\nNumber of stones: %d\n"
    (process_stones_aux 25 stones |> List.length)

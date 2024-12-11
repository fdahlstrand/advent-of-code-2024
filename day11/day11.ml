let split_stone s =
  let n = String.length s in
  [String.sub s 0 (n / 2); String.sub s (n / 2) (n / 2)]
  |> List.map int_of_string |> List.map string_of_int

let process_stone s =
  if s = "0" then ["1"]
  else if String.length s mod 2 = 1 then [string_of_int (int_of_string s * 2024)]
  else split_stone s

let rec process_stones_aux n stones =
  if n = 0 then stones
  else process_stones_aux (n - 1) (List.map process_stone stones |> List.concat)

let memo_predict_stone n s =
  let cache = Hashtbl.create 10 in
  let rec predict_stone n s =
    try Hashtbl.find cache (n, s)
    with Not_found ->
      if n = 0 then 1
      else
        let next = process_stone s in
        let cnt =
          match next with
          | [a; b] ->
              predict_stone (n - 1) a + predict_stone (n - 1) b
          | [a] ->
              predict_stone (n - 1) a
          | _ ->
              failwith "This should not happen"
        in
        Hashtbl.add cache (n, s) cnt ;
        cnt
  in
  predict_stone n s

let stones =
  In_channel.with_open_text "./input/day11/input.txt" In_channel.input_all
  |> String.trim |> String.split_on_char ' '

let () =
  Printf.printf
    "\n\
     Number of stones after 25 blinks: %d\n\
     Number of stones after 75 blinks: %d\n"
    (process_stones_aux 25 stones |> List.length)
    (List.map (memo_predict_stone 75) stones |> List.fold_left ( + ) 0)

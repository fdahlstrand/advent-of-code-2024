let swap lst = List.map (fun (a, b) -> (b, a)) lst

let numeric =
  [ ((0, 0), '7')
  ; ((0, 1), '8')
  ; ((0, 2), '9')
  ; ((1, 0), '4')
  ; ((1, 1), '5')
  ; ((1, 2), '6')
  ; ((2, 0), '1')
  ; ((2, 1), '2')
  ; ((2, 2), '3')
  ; ((3, 1), '0')
  ; ((3, 2), 'A') ]

let directional =
  [((0, 1), '^'); ((0, 2), 'A'); ((1, 0), '<'); ((1, 1), 'v'); ((1, 2), '>')]

let key_of_pos_opt keypad pos = List.assoc_opt pos keypad

let pos_of_key keypad key = List.assoc key (swap keypad)

let keypad_of_keys src dst =
  match (src, dst) with
  | 'A', 'A' ->
      directional
  | '0' .. '9', _ ->
      numeric
  | _, '0' .. '9' ->
      numeric
  | '<', _ | '^', _ | 'v', _ | '>', _ ->
      directional
  | _, '<' | _, '^' | _, 'v' | _, '>' ->
      directional
  | _ ->
      failwith "Oops"

let is_valid_pos keypad pos = Option.is_some (key_of_pos_opt keypad pos)

let distance (r, c) (r', c') = abs (r - r') + abs (c - c')

let all_moves sequence =
  let rec all_moves_aux acc = function
    | src :: dst :: rest ->
        all_moves_aux ((src, dst) :: acc) (dst :: rest)
    | _ ->
        acc |> List.rev
  in
  all_moves_aux [] ('A' :: sequence)

let rec sequence keypad src_pos tgt_pos path =
  if src_pos = tgt_pos then ['A' :: path |> List.rev]
  else
    let r, c = src_pos in
    let dist = distance (r, c) tgt_pos in
    [((r - 1, c), '^'); ((r, c + 1), '>'); ((r + 1, c), 'v'); ((r, c - 1), '<')]
    |> List.filter (fun (pos, _) -> is_valid_pos keypad pos)
    |> List.filter (fun (pos, _) -> distance pos tgt_pos < dist)
    |> List.map (fun (pos, dir) -> sequence keypad pos tgt_pos (dir :: path))
    |> List.flatten

let sequence_of_key src_key tgt_key =
  let keypad = keypad_of_keys src_key tgt_key in
  sequence keypad (pos_of_key keypad src_key) (pos_of_key keypad tgt_key) []

let count depth src_key tgt_key =
  let cache = Hashtbl.create 10 in
  let rec count_aux depth src_key tgt_key =
    if depth = 0 then 1
    else
      try Hashtbl.find cache (depth, src_key, tgt_key)
      with Not_found ->
        let sequences = sequence_of_key src_key tgt_key |> List.map all_moves in
        let sum =
          List.map
            (fun seq ->
              List.map (fun (src, tgt) -> count_aux (depth - 1) src tgt) seq
              |> List.fold_left ( + ) 0 )
            sequences
          |> List.fold_left min Int.max_int
        in
        Hashtbl.add cache (depth, src_key, tgt_key) sum ;
        sum
  in
  count_aux depth src_key tgt_key

let count_code depth code =
  code |> String.to_seq |> List.of_seq |> all_moves
  |> List.map (fun (src, tgt) -> count depth src tgt)
  |> List.fold_left ( + ) 0

let complexity depth code =
  let len = count_code depth code in
  let num = int_of_string (String.sub code 0 3) in
  num * len

let read_codes path = In_channel.with_open_text path In_channel.input_lines

let () =
  let codes = read_codes "./input/day21/input.txt" in
  let sum2 = codes |> List.map (complexity 3) |> List.fold_left ( + ) 0 in
  let sum25 = codes |> List.map (complexity 26) |> List.fold_left ( + ) 0 in
  Printf.printf
    "Complexity (2 intermediate robots): %d\n\
     Complexity (2 intermediate robots): %d\n\
     %!"
    sum2 sum25

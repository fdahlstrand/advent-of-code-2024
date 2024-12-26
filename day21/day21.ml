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

let key_of_pos keypad pos = List.assoc pos keypad

let key_of_pos_opt keypad pos = List.assoc_opt pos keypad

let pos_of_key keypad key = List.assoc key (swap keypad)

let pos_of_key_opt keypad key = List.assoc_opt key (swap keypad)

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

let all_moves sequence =
  let rec all_moves_aux acc = function
    | src :: dst :: rest ->
        all_moves_aux ((src, dst) :: acc) (dst :: rest)
    | _ ->
        acc |> List.rev
  in
  all_moves_aux [] ('A' :: sequence)

let rec dfs f_next f_reduce f_goal s =
  match f_goal s with
  | Some v ->
      v
  | None ->
      f_reduce (List.map (fun s' -> dfs f_next f_reduce f_goal s') (f_next s))

let cache = Hashtbl.create 10

type sequence_state = {src_key: char; tgt_key: char; path: char list}

type count_state = {depth: int; sequence: char list}

let count_goal {depth; _} = if depth = 0 then Some 1 else None

let sequence_goal {src_key; tgt_key; path} =
  if src_key = tgt_key then Some (List.rev ('A' :: path)) else None

let distance (r, c) (r', c') = abs (r - r') + abs (c - c')

let print_sequence_state depth label {src_key; tgt_key; path} =
  let indent = List.init depth (fun _ -> "  ") |> String.concat "" in
  let path_string = List.map (String.make 1) path |> String.concat "" in
  Printf.printf "%s%s: %c -> %c : [%s]\n%!" indent label src_key tgt_key
    path_string

let print_count_state label {depth; sequence} =
  let indent = List.init depth (fun _ -> "  ") |> String.concat "" in
  let seq_string = List.map (String.make 1) sequence |> String.concat "" in
  Printf.printf "%s%s: [%s]\n%!" indent label seq_string

let rec sequence_next depth ({src_key; tgt_key; path; _} as state) =
  (* let _ = print_sequence_state depth "next seq" state in *)
  let keypad = keypad_of_keys src_key tgt_key in
  let r, c = pos_of_key keypad src_key in
  let r', c' = pos_of_key keypad tgt_key in
  let d = distance (r, c) (r', c') in
  [((r - 1, c), '^'); ((r, c + 1), '>'); ((r + 1, c), 'v'); ((r, c - 1), '<')]
  |> List.filter (fun (pos, _) -> Option.is_some (key_of_pos_opt keypad pos))
  |> List.filter (fun (pos, _) -> distance pos (r', c') < d)
  |> List.map (fun (pos, dir) ->
         {state with src_key= key_of_pos keypad pos; path= dir :: path} )

and sequence_reduce depth lst =
  List.map (fun seq -> (count_moves depth seq, seq)) lst
  |> List.fold_left
       (fun (cnt, seq) (cnt', seq') ->
         if cnt' < cnt then (cnt', seq') else (cnt, seq) )
       (Int.max_int, [])
  |> snd

and count_next ({sequence; depth} as state) =
  (* let _ = print_count_state "next count" state in *)
  (* let _ = read_line () in *)
  let moves = all_moves sequence in
  (* let _ = *)
  (*   let m_str = List.map (fun (a, b) -> Printf.sprintf "(%c,%c)" a b) moves in *)
  (*   Printf.printf ">> %s\n" (m_str |> String.concat ", ") *)
  (* in *)
  let depth' = depth - 1 in
  moves
  |> List.map (fun (src, dst) ->
         {depth= depth'; sequence= get_sequence depth' src dst} )

and count_reduce lst = List.fold_left ( + ) 0 lst

and get_sequence depth src_key tgt_key =
  dfs (sequence_next depth) (sequence_reduce depth) sequence_goal
    {src_key; tgt_key; path= []}

and count_moves depth sequence =
  try Hashtbl.find cache (depth, sequence)
  with Not_found ->
    let count = dfs count_next count_reduce count_goal {depth; sequence} in
    Hashtbl.add cache (depth, sequence) count ;
    count

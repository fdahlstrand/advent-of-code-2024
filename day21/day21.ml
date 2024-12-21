let numeric_keypad =
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

let directional_keypad =
  [((0, 1), '^'); ((0, 2), 'A'); ((1, 0), '<'); ((1, 1), 'v'); ((1, 2), '>')]

let distance (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

let pos_of_key keypad key =
  List.filter (fun (_, k) -> k = key) keypad |> List.hd |> fst

let cost keypad =
  let keys = List.map snd keypad in
  let moves =
    List.concat (List.map (fun a -> List.map (fun b -> (a, b)) keys) keys)
  in
  moves
  |> List.map (fun (k1, k2) ->
         ((k1, k2), distance (pos_of_key keypad k1) (pos_of_key keypad k2)) )

let move keypad pos_key goal_key =
  let rec move_aux keypad ((rp, cp) as pos) ((rg, cg) as goal) path =
    if pos = goal then path
    else if cp > cg && List.mem_assoc (rp, cp - 1) keypad then
      move_aux keypad (rp, cp - 1) goal ('<' :: path)
    else if rp < rg && List.mem_assoc (rp + 1, cp) keypad then
      move_aux keypad (rp + 1, cp) goal ('v' :: path)
    else if rp > rg && List.mem_assoc (rp - 1, cp) keypad then
      move_aux keypad (rp - 1, cp) goal ('^' :: path)
    else if cp < cg && List.mem_assoc (rp, cp + 1) keypad then
      move_aux keypad (rp, cp + 1) goal ('>' :: path)
    else
      failwith
        (Printf.sprintf "Cannot move from (%d,%d) to (%d,%d)" rp cp rg cg)
  in
  move_aux keypad (pos_of_key keypad pos_key) (pos_of_key keypad goal_key) []
  |> List.rev

let _sequence keypad keys =
  let rec sequence_aux keypad sequence cur_key = function
    | next_key :: rest ->
        let m = move keypad cur_key next_key in
        sequence_aux keypad ((m @ ['A']) :: sequence) next_key rest
    | [] ->
        sequence |> List.rev |> List.flatten
  in
  sequence_aux keypad [] 'A' keys

let all_moves keypad cur_key goal_key =
  let rec all_moves_aux keypad (rp, cp) (rg, cg) =
    if rp = rg && cp = cg then [[]]
    else
      let dist = distance (rp, cp) (rg, cg) in
      let next =
        [ ((rp - 1, cp), '^')
        ; ((rp, cp + 1), '>')
        ; ((rp + 1, cp), 'v')
        ; ((rp, cp - 1), '<') ]
        |> List.filter (fun (p, _) -> List.mem_assoc p keypad)
        |> List.filter (fun (p, _) -> distance p (rg, cg) < dist)
      in
      List.map
        (fun ((r, c), k) ->
          List.map (fun p -> k :: p) (all_moves_aux keypad (r, c) (rg, cg)) )
        next
      |> List.flatten
  in
  all_moves_aux keypad (pos_of_key keypad cur_key) (pos_of_key keypad goal_key)

let rec all_sequences_aux keypad cur_key = function
  | next_key :: rest ->
      let all_moves = all_moves keypad cur_key next_key in
      List.map
        (fun m ->
          List.map
            (fun s -> (m @ ['A']) @ s)
            (all_sequences_aux keypad next_key rest) )
        all_moves
      |> List.flatten
  | [] ->
      [[]]

let rec all_pairs acc = function
  | a :: b :: rest ->
      all_pairs ((a, b) :: acc) (b :: rest)
  | _ ->
      acc |> List.rev

let estimate_cost keypad sequence =
  let cs = cost keypad in
  List.map
    (fun s ->
      ( List.map (fun p -> List.assoc p cs) (all_pairs [] s)
        |> List.fold_left ( + ) 0
      , s ) )
    sequence

let min_cost keypad sequence =
  let cs = cost keypad in
  List.map
    (fun s ->
      List.map (fun p -> List.assoc p cs) (all_pairs [] s)
      |> List.fold_left ( + ) 0 )
    sequence
  |> List.fold_left min Int.max_int

let shortest_seq sequences =
  let c_min = min_cost directional_keypad sequences in
  estimate_cost directional_keypad sequences
  |> List.filter (fun (c, _) -> c = c_min)
  |> List.map snd

let complexity code =
  let code_seq = code |> String.to_seq |> List.of_seq in
  let a = all_sequences_aux numeric_keypad 'A' code_seq |> shortest_seq in
  let _ = Printf.printf "A\n%!" in
  let b =
    List.map (fun s -> all_sequences_aux directional_keypad 'A' s) a
    |> List.flatten |> shortest_seq
  in
  let _ = Printf.printf "B\n%!" in
  let c =
    List.map (fun s -> all_sequences_aux directional_keypad 'A' s) b
    |> List.flatten
  in
  let len = List.map List.length c |> List.fold_left min Int.max_int in
  let num = int_of_string (String.sub code 0 3) in
  let _ = Printf.printf "C\n%!" in
  len * num

let () =
  let cplx =
    ["539A"; "964A"; "803A"; "149A"; "789A"]
    |> List.map complexity |> List.fold_left ( + ) 0
  in
  Printf.printf "Complexity: %d\n%!" cplx

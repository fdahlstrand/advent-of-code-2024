let modulo x y =
  let r = x mod y in
  if r >= 0 then r else r + y

let make_robot w h (px, py) (vx, vy) =
  let p' n = (modulo (px + (n * vx)) w, modulo (py + (n * vy)) h) in
  p'

let quadrant w h (x, y) =
  let mx = w / 2 in
  let my = h / 2 in
  if x < mx && y < my then Some 1
  else if x > mx && y < my then Some 2
  else if x < mx && y > my then Some 3
  else if x > mx && y > my then Some 4
  else None

let read_instructions path =
  let parse_instruction s =
    let rexp =
      Str.regexp {|p=\(-?[0-9]+\),\(-?[0-9]+\) v=\(-?[0-9]+\),\(-?[0-9]+\)|}
    in
    if Str.string_match rexp s 0 then
      ( ( int_of_string (Str.matched_group 1 s)
        , int_of_string (Str.matched_group 2 s) )
      , ( int_of_string (Str.matched_group 3 s)
        , int_of_string (Str.matched_group 4 s) ) )
    else failwith ("Parse error: " ^ s)
  in
  In_channel.with_open_text path In_channel.input_lines
  |> List.map parse_instruction

let make_robots w h instructions =
  let f = make_robot w h in
  List.map (fun (p, v) -> f p v) instructions

let calculate_safety_score w h path duration =
  let robots =
    read_instructions path |> make_robots w h |> List.map (fun r -> r duration)
  in
  let partitions = robots |> List.filter_map (quadrant w h) in
  [1; 2; 3; 4]
  |> List.map (fun n -> List.partition (fun q -> q = n) partitions)
  |> List.map fst |> List.map List.length |> List.fold_left ( * ) 1

let () =
  Printf.printf "\nSafety score: %d\n%!"
    (calculate_safety_score 101 103 "./input/day14/input.txt" 100)

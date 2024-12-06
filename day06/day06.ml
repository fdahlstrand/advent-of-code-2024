type map_element = Obstacle | Empty | Guard

type direction = North | South | East | West

module Position = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    if Stdlib.compare x1 x2 = 0 then Stdlib.compare y1 y2
    else Stdlib.compare x1 x2
end

module Guard = struct
  type t = int * int * direction

  let compare (r1, c1, d1) (r2, c2, d2) =
    if Stdlib.compare r1 r2 = 0 then
      if Stdlib.compare c1 c2 = 0 then Stdlib.compare d1 d2
      else Stdlib.compare c1 c2
    else Stdlib.compare r1 r2
end

module TheMap = Map.Make (Position)
module VisitSet = Set.Make (Position)
module GuardSet = Set.Make (Guard)

let turn_right (r, c, d) =
  let d' =
    match d with North -> East | East -> South | South -> West | West -> North
  in
  (r, c, d')

let next_pos g =
  let r, c, d = g in
  match d with
  | North ->
      (r - 1, c)
  | East ->
      (r, c + 1)
  | South ->
      (r + 1, c)
  | West ->
      (r, c - 1)

let move g =
  let _, _, d = g in
  let r', c' = next_pos g in
  (r', c', d)

let look_ahead m g =
  let r', c' = next_pos g in
  TheMap.find_opt (r', c') m

let read_map path =
  In_channel.with_open_text path In_channel.input_lines
  |> List.mapi (fun row s ->
         Seq.mapi
           (fun col ch ->
             match ch with
             | '#' ->
                 ((row, col), Obstacle)
             | '^' ->
                 ((row, col), Guard)
             | _ ->
                 ((row, col), Empty) )
           (String.to_seq s) )
  |> List.map List.of_seq |> List.flatten |> TheMap.of_list

let extract_guard m =
  let (row, col), _ =
    TheMap.filter (fun _ v -> match v with Guard -> true | _ -> false) m
    |> TheMap.choose
  in
  ((row, col, North), TheMap.update (row, col) (fun _ -> Some Empty) m)

let walk g m =
  let rec walk' visited g m =
    let row, col, _ = g in
    let pos = (row, col) in
    let visited' = VisitSet.add pos visited in
    match look_ahead m g with
    | Some Obstacle ->
        walk' visited' (turn_right g) m
    | Some Empty ->
        walk' visited' (move g) m
    | None ->
        visited'
    | _ ->
        failwith "Unexpected map element"
  in
  walk' VisitSet.empty g m

let detect_loop g m =
  let rec detect_loop' guards g m =
    if GuardSet.mem g guards then true
    else
      match look_ahead m g with
      | Some Obstacle ->
          detect_loop' (GuardSet.add g guards) (turn_right g) m
      | Some Empty ->
          detect_loop' (GuardSet.add g guards) (move g) m
      | None ->
          false
      | _ ->
          failwith "Unexpected map element"
  in
  detect_loop' GuardSet.empty g m

let empty_positions m =
  TheMap.filter (fun _ v -> match v with Empty -> true | _ -> false) m
  |> TheMap.bindings
  |> List.map (fun (k, _) -> k)

let possible_obstacle_positions g m =
  empty_positions m
  |> List.filter (fun pos ->
         let m' = TheMap.update pos (fun _ -> Some Obstacle) m in
         detect_loop g m' )

let g, m = read_map "./input/day06/input.txt" |> extract_guard

let nbr_of_steps = walk g m |> VisitSet.cardinal

let nbr_of_obstacles = possible_obstacle_positions g m |> List.length

let () =
  Printf.printf "\nNbr of steps: %d\nPossible obstacle positions: %d\n"
    nbr_of_steps nbr_of_obstacles

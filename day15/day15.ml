module Pos = struct
  type t = int * int

  let compare (r1, c1) (r2, c2) =
    if Stdlib.compare r1 r2 = 0 then Stdlib.compare c1 c2
    else Stdlib.compare r1 r2
end

module PosMap = Map.Make (Pos)

type map_element = Wall | Box | Robot

type direction = Up | Right | Down | Left

type warehouse_map = {width: int; height: int; elements: map_element PosMap.t}

let read_input path =
  let parse_map s =
    String.trim s |> String.split_on_char '\n'
    |> List.mapi (fun row line ->
           List.mapi
             (fun col -> function
               | '#' ->
                   Some ((row, col), Wall)
               | 'O' ->
                   Some ((row, col), Box)
               | '@' ->
                   Some ((row, col), Robot)
               | _ ->
                   None )
             (String.to_seq line |> List.of_seq) )
    |> List.flatten
    |> List.filter_map (fun x -> x)
  in
  let parse_instr s =
    String.trim s
    |> Str.global_replace (Str.regexp "\n") ""
    |> String.to_seq |> List.of_seq
    |> List.map (function
         | '^' ->
             Up
         | '>' ->
             Right
         | 'v' ->
             Down
         | '<' ->
             Left
         | ch ->
             failwith ("Unexpected instruction: " ^ String.make 1 ch) )
  in
  let input =
    In_channel.with_open_text path In_channel.input_all
    |> Str.split (Str.regexp "\n\n")
  in
  let m, i =
    match input with [m; i] -> (m, i) | _ -> failwith "Invalid input file"
  in
  (parse_map m, parse_instr i)

let _render_map {width; height; elements} =
  let f row col =
    match PosMap.find_opt (row, col) elements with
    | Some Wall ->
        '$'
    | Some Box ->
        'O'
    | Some Robot ->
        '@'
    | None ->
        ' '
  in
  Array.init_matrix width height f
  |> Array.map (fun a -> Array.to_seq a |> String.of_seq)
  |> Array.to_list |> String.concat "\n"

let make_warehouse_map lst =
  let max_col = List.fold_left max 0 (List.map (fun ((_, c), _) -> c) lst) in
  let max_row = List.fold_left max 0 (List.map (fun ((r, _), _) -> r) lst) in
  {width= max_col + 1; height= max_row + 1; elements= PosMap.of_list lst}

let find_robot map =
  PosMap.filter (fun _ v -> v = Robot) map.elements
  |> PosMap.bindings |> List.hd |> fst

let next (r, c) = function
  | Up ->
      (r - 1, c)
  | Right ->
      (r, c + 1)
  | Down ->
      (r + 1, c)
  | Left ->
      (r, c - 1)

let is_empty pos map =
  match PosMap.find_opt pos map.elements with None -> true | _ -> false

let rec move map pos dir =
  match PosMap.find_opt pos map.elements with
  | Some Wall ->
      map
  | Some Box ->
      let pos' = next pos dir in
      let map' = move map pos' dir in
      if is_empty pos' map' then
        { map' with
          elements= PosMap.remove pos map'.elements |> PosMap.add pos' Box }
      else map
  | Some Robot ->
      let pos' = next pos dir in
      let map' = move map pos' dir in
      if is_empty pos' map' then
        { map' with
          elements= PosMap.remove pos map'.elements |> PosMap.add pos' Robot }
      else map
  | None ->
      map

let rec process map = function
  | i :: t ->
      let map' = move map (find_robot map) i in
      process map' t
  | _ ->
      map

let gps_coordinate (r, c) = (r * 100) + c

let sum_of_coordinates map =
  PosMap.filter (fun _ e -> e = Box) map.elements
  |> PosMap.bindings |> List.map fst |> List.map gps_coordinate
  |> List.fold_left ( + ) 0

let map, instructions =
  let e, i = read_input "./input/day15/input.txt" in
  (make_warehouse_map e, i)

let () =
  Printf.printf "\nSum of GPS coordinates: %d\n%!"
    (process map instructions |> sum_of_coordinates)

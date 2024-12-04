module Coord = struct
  type t = int * int

  let compare a b = if a = b then 0 else if a < b then -1 else 1
end

module CoordMap = Map.Make (Coord)

let input_path = "./input/day04/input.txt"

let word_puzzle path =
  let m =
    In_channel.with_open_text path In_channel.input_lines
    |> List.map (fun s -> String.to_seq s |> List.of_seq)
    |> List.mapi (fun row lst -> List.mapi (fun col ch -> ((row, col), ch)) lst)
    |> List.flatten |> CoordMap.of_list
  in
  (* let (x, y), _ = CoordMap.max_binding m in *)
  m

let trace (r, c) (dr, dc) m =
  let rec trace' (r, c) (dr, dc) tr m =
    match CoordMap.find_opt (r, c) m with
    | Some ch ->
        trace' (r + dr, c + dc) (dr, dc) (tr @ [ch]) m
    | None ->
        tr
  in
  trace' (r, c) (dr, dc) [] m

let border f m =
  CoordMap.bindings m
  |> List.filter (fun (k, _) -> f k)
  |> List.map (fun (k, _) -> k)

let top m = border (fun (r, _) -> r = 0) m

let left m = border (fun (_, c) -> c = 0) m

let top_left m = border (fun (r, c) -> r = 0 || c = 0) m

let top_right m =
  let (_, max_col), _ = CoordMap.max_binding m in
  border (fun (r, c) -> r = 0 || c = max_col) m

let collect_traces f offset m =
  let rec collect_traces' coords offset tr m =
    match coords with
    | h :: t ->
        collect_traces' t offset (trace h offset m :: tr) m
    | [] ->
        tr
  in
  collect_traces' (f m) offset [] m

let count lst =
  lst |> List.map List.to_seq |> List.map String.of_seq
  |> List.map (fun s -> Str.full_split (Str.regexp {|XMAS|}) s)
  |> List.map
       (List.filter (fun d -> match d with Str.Delim _ -> true | _ -> false))
  |> List.map List.length |> List.fold_left ( + ) 0

let puzzle = word_puzzle input_path

let string_of_coords lst m =
  let rec string_of_coords' chs lst m =
    match lst with
    | h :: t -> (
      match CoordMap.find_opt h m with
      | Some ch ->
          string_of_coords' (chs @ [ch]) t m
      | None ->
          string_of_coords' chs t m )
    | [] ->
        chs
  in
  string_of_coords' [] lst m |> List.to_seq |> String.of_seq

let x_mas (r, c) m =
  let s1 = string_of_coords [(r - 1, c - 1); (r, c); (r + 1, c + 1)] m in
  let s2 = string_of_coords [(r - 1, c + 1); (r, c); (r + 1, c - 1)] m in
  (s1 = "MAS" || s1 = "SAM") && (s2 = "MAS" || s2 = "SAM")

let horizontal =
  let h = collect_traces top (1, 0) puzzle in
  h @ List.map List.rev h

let vertical =
  let v = collect_traces left (0, 1) puzzle in
  v @ List.map List.rev v

let diagonal1 =
  let d = collect_traces top_left (1, 1) puzzle in
  d @ List.map List.rev d

let diagonal2 =
  let d = collect_traces top_right (1, -1) puzzle in
  d @ List.map List.rev d

let total =
  count horizontal + count vertical + count diagonal1 + count diagonal2

let total_x_mas =
  CoordMap.filter (fun _ v -> v = 'A') puzzle
  |> CoordMap.bindings
  |> List.map (fun (k, _) -> k)
  |> List.map (fun c -> if x_mas c puzzle then 1 else 0)
  |> List.fold_left ( + ) 0

let () =
  Printf.printf "\nTotal XMAS found: %d\nTotal X-MAS found: %d\n" total
    total_x_mas

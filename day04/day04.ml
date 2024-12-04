module Coord = struct
  type t = int * int

  let compare a b = if a = b then 0 else if a < b then -1 else 1
end

module CoordMap = Map.Make (Coord)

let input_path = "./input/day04/input.txt"

let word_puzzle path =
  In_channel.with_open_text path In_channel.input_lines
  |> List.map (fun s -> String.to_seq s |> List.of_seq)
  |> List.mapi (fun row lst -> List.mapi (fun col ch -> ((row, col), ch)) lst)
  |> List.flatten |> CoordMap.of_list

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

let xmas (r, c) m =
  let ss =
    [ string_of_coords [(r, c); (r + 1, c); (r + 2, c); (r + 3, c)] m
    ; string_of_coords [(r, c); (r - 1, c); (r - 2, c); (r - 3, c)] m
    ; string_of_coords [(r, c); (r, c + 1); (r, c + 2); (r, c + 3)] m
    ; string_of_coords [(r, c); (r, c - 1); (r, c - 2); (r, c - 3)] m
    ; string_of_coords
        [(r, c); (r - 1, c - 1); (r - 2, c - 2); (r - 3, c - 3)]
        m
    ; string_of_coords
        [(r, c); (r + 1, c + 1); (r + 2, c + 2); (r + 3, c + 3)]
        m
    ; string_of_coords
        [(r, c); (r - 1, c + 1); (r - 2, c + 2); (r - 3, c + 3)]
        m
    ; string_of_coords
        [(r, c); (r + 1, c - 1); (r + 2, c - 2); (r + 3, c - 3)]
        m ]
  in
  ss
  |> List.map (fun s -> if s = "XMAS" || s = "SAMX" then 1 else 0)
  |> List.fold_left ( + ) 0

let x_mas (r, c) m =
  let s1 = string_of_coords [(r - 1, c - 1); (r, c); (r + 1, c + 1)] m in
  let s2 = string_of_coords [(r - 1, c + 1); (r, c); (r + 1, c - 1)] m in
  if (s1 = "MAS" || s1 = "SAM") && (s2 = "MAS" || s2 = "SAM") then 1 else 0

let search ch f m =
  CoordMap.filter (fun _ v -> v = ch) m
  |> CoordMap.bindings
  |> List.map (fun (k, _) -> k)
  |> List.map (fun c -> f c puzzle)
  |> List.fold_left ( + ) 0

let total_xmas = search 'X' xmas puzzle

let total_x_mas = search 'A' x_mas puzzle

let () =
  Printf.printf "\nTotal XMAS found: %d\nTotal X-MAS found: %d\n" total_xmas
    total_x_mas

type pos = {row: int; col: int}

type antenna = {freq: char; pos: pos}

module PosSet = Set.Make (struct
  type t = pos

  let compare {row= r1; col= c1} {row= r2; col= c2} =
    if Stdlib.compare r1 r2 = 0 then Stdlib.compare c1 c2
    else Stdlib.compare r1 r2
end)

let ( -- ) {row= r1; col= c1} {row= r2; col= c2} = {row= r2 - r1; col= c2 - c1}

let ( ++ ) {row= r1; col= c1} {row= r2; col= c2} = {row= r2 + r1; col= c2 + c1}

let read_map path =
  In_channel.with_open_text path In_channel.input_lines
  |> List.mapi (fun row lst ->
         List.mapi
           (fun col ch -> {freq= ch; pos= {row; col}})
           (String.to_seq lst |> List.of_seq) )
  |> List.flatten

let map_size m =
  let rec map_size' max_row max_col = function
    | {pos= {row; col}; _} :: t ->
        map_size' (max row max_row) (max col max_col) t
    | [] ->
        (max_col + 1, max_row + 1)
  in
  map_size' (-1) (-1) m

let frequencies_of_map m =
  let rec frequencies_of_map' acc lst =
    match lst with
    | h :: _ ->
        let {freq= hf; _} = h in
        let part = List.partition (fun {freq= f; _} -> f = hf) lst in
        frequencies_of_map' (fst part :: acc) (snd part)
    | [] ->
        acc
  in
  frequencies_of_map' [] m

let pairs_of_list lst =
  let rec make_pairs acc a = function
    | h :: t ->
        make_pairs ((a, h) :: acc) a t
    | [] ->
        List.rev acc
  in
  let rec pairs_of_list' acc = function
    | h :: t ->
        pairs_of_list' (make_pairs [] h t :: acc) t
    | [] ->
        List.rev acc |> List.flatten
  in
  pairs_of_list' [] lst

let is_on_map width height {row; col} =
  (0 <= row && row < width) && 0 <= col && col < height

let antinodes width height {pos= p1; _} {pos= p2; _} =
  let an1 = p1 ++ (p2 -- p1) in
  let an2 = p2 ++ (p1 -- p2) in
  (if is_on_map width height an1 then [an1] else [])
  @ if is_on_map width height an2 then [an2] else []

let harmonics width height {pos= p1; _} {pos= p2; _} =
  let rec harmonics' width height p dp =
    if is_on_map width height p then p :: harmonics' width height (p ++ dp) dp
    else []
  in
  let dp1 = p1 -- p2 in
  let dp2 = p2 -- p1 in
  harmonics' width height p2 dp1 @ harmonics' width height p1 dp2

let calculate_antinodes f path =
  let m = read_map path in
  let w, h = map_size m in
  List.filter (fun {freq= f; _} -> f != '.') m
  |> frequencies_of_map
  |> List.map (fun lst -> pairs_of_list lst)
  |> List.flatten
  |> List.map (fun (a, b) -> f w h a b)
  |> List.flatten |> PosSet.of_list |> PosSet.cardinal

let input_path = "./input/day08/input.txt"

let antinode_count = calculate_antinodes antinodes input_path

let harmonics_count = calculate_antinodes harmonics input_path

let () =
  Printf.printf
    "\n\
     Nbr of locations with an antinode: %d\n\
     Nbr of locations with harmonics: %d\n"
    antinode_count harmonics_count

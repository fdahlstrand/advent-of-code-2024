type pos = {row: int; col: int}

module Pos = struct
  type t = pos

  let compare {row= r1; col= c1} {row= r2; col= c2} =
    if Stdlib.compare r1 r2 = 0 then Stdlib.compare c1 c2
    else Stdlib.compare r1 r2
end

module PosSet = Set.Make (Pos)
module PosMap = Map.Make (Pos)

let read_topographic_map path =
  In_channel.with_open_text path In_channel.input_lines
  |> List.mapi (fun row s ->
         List.mapi
           (fun col ch -> ({row; col}, int_of_char ch - int_of_char '0'))
           (String.to_seq s |> List.of_seq) )
  |> List.flatten |> PosMap.of_list

let trail_heads m =
  PosMap.filter (fun _ h -> h = 0) m |> PosMap.bindings |> List.map fst

let next m h {row; col} =
  [ {row= row - 1; col}
  ; {row; col= col + 1}
  ; {row= row + 1; col}
  ; {row; col= col - 1} ]
  |> List.filter (fun p ->
         match PosMap.find_opt p m with Some h' -> h' = h + 1 | None -> false )

let walk m p =
  let rec walk' m tops visited p =
    let h = PosMap.find p m in
    let pn = next m h p in
    if PosSet.mem p visited then tops
    else if h = 9 then PosSet.add p tops
    else
      let visited' = PosSet.add p visited in
      pn
      |> List.map (walk' m tops visited')
      |> List.fold_left PosSet.union PosSet.empty
  in
  walk' m PosSet.empty PosSet.empty p |> PosSet.cardinal

let trails m p =
  let rec trails' m trails visited p =
    let h = PosMap.find p m in
    let pn = next m h p in
    if PosSet.mem p visited then trails
    else if h = 9 then [p]
    else
      let visited' = PosSet.add p visited in
      pn |> List.map (trails' m trails visited') |> List.concat
  in
  trails' m [] PosSet.empty p

let total_score =
  let m = read_topographic_map "./input/day10/input.txt" in
  trail_heads m |> List.map (walk m) |> List.fold_left ( + ) 0

let total_rating =
  let m = read_topographic_map "./input/day10/input.txt" in
  trail_heads m
  |> List.map (trails m)
  |> List.map List.length |> List.fold_left ( + ) 0

let () =
  Printf.printf "\nTotal score: %d\nTotal rating: %d\n" total_score total_rating

type pos = {row: int; col: int}

module Pos = struct
  type t = pos

  let compare {row= r1; col= c1} {row= r2; col= c2} =
    if Stdlib.compare r1 r2 = 0 then Stdlib.compare c1 c2
    else Stdlib.compare r1 r2
end

module GardenMap = Map.Make (Pos)
module PosSet = Set.Make (Pos)

let read_garden_map path =
  In_channel.with_open_text path In_channel.input_lines
  |> List.mapi (fun row s ->
         List.mapi
           (fun col ch -> ({row; col}, ch))
           (s |> String.to_seq |> List.of_seq) )
  |> List.flatten |> GardenMap.of_list

let surround {row; col} =
  [ {row= row - 1; col}
  ; {row; col= col + 1}
  ; {row= row + 1; col}
  ; {row; col= col - 1} ]

let next m ch p =
  let pn = surround p in
  List.filter
    (fun p' ->
      match GardenMap.find_opt p' m with Some x -> ch = x | None -> false )
    pn

let collect_region m p =
  let rec collect_region_aux m ch visisted p =
    let pn =
      next m ch p |> List.filter (fun p' -> not (PosSet.mem p' visisted))
    in
    let visited' = PosSet.add p visisted in
    List.fold_left (collect_region_aux m ch) visited' pn
  in
  collect_region_aux m (GardenMap.find p m) PosSet.empty p

let collect_regions m =
  let rec collect_regions_aux m visited regions plants =
    match plants with
    | (pos, ch) :: t ->
        if PosSet.mem pos visited then collect_regions_aux m visited regions t
        else
          let region = collect_region m pos in
          let regions' = (ch, region) :: regions in
          collect_regions_aux m (PosSet.union visited region) regions' t
    | [] ->
        regions
  in
  collect_regions_aux m PosSet.empty [] (GardenMap.bindings m)

let north region {row; col} = not (PosSet.mem {row= row - 1; col} region)

let east region {row; col} = not (PosSet.mem {row; col= col + 1} region)

let south region {row; col} = not (PosSet.mem {row= row + 1; col} region)

let west region {row; col} = not (PosSet.mem {row; col= col - 1} region)

let horizontal {row; col} = [{row; col= col - 1}; {row; col= col + 1}]

let vertical {row; col} = [{row= row - 1; col}; {row= row + 1; col}]

let trace_fence region fd fc p =
  let rec trace_fence_aux region visited p =
    let pn =
      fd p
      |> List.filter (fun p' ->
             PosSet.mem p' region
             && (not (PosSet.mem p' visited))
             && fc region p' )
    in
    let visited' = PosSet.add p visited in
    List.fold_left (trace_fence_aux region) visited' pn
  in
  trace_fence_aux region PosSet.empty p

let find_fences region =
  let rec find_fences_aux region fd fc visited fences plants =
    match plants with
    | p :: t ->
        if PosSet.mem p visited then
          find_fences_aux region fd fc visited fences t
        else
          let fence = trace_fence region fd fc p in
          let fences' = fence :: fences in
          find_fences_aux region fd fc (PosSet.union visited fence) fences' t
    | [] ->
        fences
  in
  let find fd fc =
    find_fences_aux region fd fc PosSet.empty []
      (PosSet.filter (fc region) region |> PosSet.to_list)
  in
  [ find horizontal north
  ; find vertical east
  ; find horizontal south
  ; find vertical west ]

let count_sides region =
  find_fences region |> List.map List.length |> List.fold_left ( + ) 0

let area region = PosSet.cardinal region

let perimeter region =
  let rec perimeter_aux region acc = function
    | p :: t ->
        let ps = surround p |> List.filter (fun p' -> PosSet.mem p' region) in
        let sides = 4 - List.length ps in
        perimeter_aux region (acc + sides) t
    | [] ->
        acc
  in
  perimeter_aux region 0 (region |> PosSet.to_list)

let price =
  read_garden_map "./input/day12/input.txt"
  |> collect_regions
  |> List.map (fun (_, r) -> area r * perimeter r)
  |> List.fold_left ( + ) 0

let fence_price =
  read_garden_map "./input/day12/input.txt"
  |> collect_regions
  |> List.map (fun (_, r) -> area r * count_sides r)
  |> List.fold_left ( + ) 0

let () =
  Printf.printf "\nFence cost: %d\nFence cost (sides): %d\n" price fence_price

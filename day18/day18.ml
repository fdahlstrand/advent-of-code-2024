module PrioQueue = struct
  type prio = int

  type 'a queue = Empty | Node of prio * 'a * 'a queue * 'a queue

  let empty = Empty

  exception Queue_empty

  let rec insert prio elt queue =
    match queue with
    | Empty ->
        Node (prio, elt, Empty, Empty)
    | Node (p, e, left, right) ->
        if prio <= p then Node (prio, elt, insert p e right, left)
        else Node (p, e, insert prio elt right, left)

  let rec remove_top = function
    | Empty ->
        raise Queue_empty
    | Node (_, _, left, Empty) ->
        left
    | Node (_, _, Empty, right) ->
        right
    | Node
        ( _
        , _
        , (Node (lprio, lelt, _, _) as left)
        , (Node (rprio, relt, _, _) as right) ) ->
        if lprio <= rprio then Node (lprio, lelt, remove_top left, right)
        else Node (rprio, relt, left, remove_top right)

  let extract_min = function
    | Empty ->
        raise Queue_empty
    | Node (prio, elt, _, _) as queue ->
        (prio, elt, remove_top queue)

  let is_empty = function Empty -> true | _ -> false
end

module Pos = struct
  type t = int * int

  let compare = Stdlib.compare
end

module PosMap = Map.Make (Pos)
module PosSet = Set.Make (Pos)

type coord_state = Clear | Corrupted

type memory_space = {width: int; height: int; space: coord_state PosMap.t}

let make_memory_space width height =
  let space =
    Array.make_matrix width height Clear
    |> Array.mapi (fun row arr -> Array.mapi (fun col s -> ((row, col), s)) arr)
    |> Array.map Array.to_list |> Array.to_list |> List.flatten
    |> PosMap.of_list
  in
  {width; height; space}

let set_corrupted coord ms =
  {ms with space= PosMap.update coord (Option.map (fun _ -> Corrupted)) ms.space}

let read_coords path =
  let parse_coord s =
    if Str.string_match (Str.regexp {|\([0-9]+\),\([0-9]+\)|}) s 0 then
      ( int_of_string (Str.matched_group 2 s)
      , int_of_string (Str.matched_group 1 s) )
    else failwith "Parse Error: Invalid coordinate"
  in
  In_channel.with_open_text path In_channel.input_lines
  |> List.map String.trim |> List.map parse_coord

let take n lst =
  let rec take_aux n acc = function
    | h :: t ->
        if n = 0 then acc else take_aux (n - 1) (h :: acc) t
    | [] ->
        acc
  in
  take_aux n [] lst |> List.rev

let _render_memory_space {width; height; space} =
  let f x y =
    match PosMap.find (x, y) space with Corrupted -> "#" | Clear -> "."
  in
  Array.init_matrix width height f
  |> Array.map Array.to_list
  |> Array.map (String.concat "")
  |> Array.to_list |> String.concat "\n"

let neighbours map (row, col) =
  [(row - 1, col); (row, col + 1); (row + 1, col); (row, col - 1)]
  |> List.filter (fun p ->
         match PosMap.find_opt p map.space with
         | Some e ->
             e = Clear
         | None ->
             false )

let manhattan (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

let a_star start goal h neighbours =
  let rec reconstruct_path trails current path =
    if PosMap.mem current trails then
      let next = PosMap.find current trails in
      reconstruct_path trails next (current :: path)
    else path
  in
  let g_score s p =
    match PosMap.find_opt p s with Some g -> g | None -> Int.max_int
  in
  let rec a_star' open_set seen g_scores trails =
    if PrioQueue.is_empty open_set then []
    else
      (* let _ = Printf.printf "==========\n" in *)
      let g_score = g_score g_scores in
      let _, current, open_set' = PrioQueue.extract_min open_set in
      if current = goal then reconstruct_path trails current []
      else
        let tentative_gscore = g_score current + 1 in
        let seen' = PosSet.add current seen in
        let ns =
          neighbours current
          |> List.filter (fun p -> not (PosSet.mem p seen))
          |> List.map (fun p -> (p, g_score p))
        in
        (* let _ = *)
        (*   List.iter *)
        (*     (fun ((r, c), gs) -> Printf.printf "> (%d,%d) [%d]\n%!" r c gs) *)
        (*     ns *)
        (* in *)
        (* let _ = read_line () in *)
        let g_scores' =
          List.fold_left
            (fun acc (p, gs) ->
              if tentative_gscore < gs then
                PosMap.update p (fun _ -> Some tentative_gscore) acc
              else acc )
            g_scores ns
        in
        let trails' =
          List.fold_left
            (fun acc (p, gs) ->
              if tentative_gscore < gs then
                PosMap.update p (fun _ -> Some current) acc
              else acc )
            trails ns
        in
        let open_set' =
          List.fold_left
            (fun acc (p, gs) ->
              if tentative_gscore < gs then
                PrioQueue.insert (tentative_gscore + h p) p acc
              else acc )
            open_set' ns
        in
        a_star' open_set' seen' g_scores' trails'
  in
  a_star'
    PrioQueue.(empty |> insert (h start) start)
    PosSet.empty
    PosMap.(empty |> add start 0)
    PosMap.(empty)

let has_path coords n =
  let space =
    coords |> take n
    |> List.fold_left
         (fun spc coord -> set_corrupted coord spc)
         (make_memory_space 71 71)
  in
  let path = a_star (0, 0) (70, 70) (manhattan (0, 0)) (neighbours space) in
  List.length path > 0

let rec find_blocker coords n m =
  if m - n <= 1 then (m, List.nth coords n)
  else
    let p = n + ((m - n) / 2) in
    if has_path coords p then find_blocker coords p m
    else find_blocker coords n p

let coords = read_coords "./input/day18/input.txt"

let space =
  coords |> take 1024
  |> List.fold_left
       (fun spc coord -> set_corrupted coord spc)
       (make_memory_space 71 71)

let () = print_endline (_render_memory_space space)

let path = a_star (0, 0) (70, 70) (manhattan (0, 0)) (neighbours space)

let () = Printf.printf "Minimal path: %d\n%!" (List.length path)

let _, (y, x) = find_blocker coords 0 3500

let () = Printf.printf "Coords of blocking byte %d,%d" x y

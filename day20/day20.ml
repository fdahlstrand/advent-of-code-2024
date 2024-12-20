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

  let rec find_opt elt = function
    | Empty ->
        None
    | Node (p, e, _, _) when e = elt ->
        Some p
    | Node (_, _, left, right) -> (
      match find_opt elt left with
      | Some p ->
          Some p
      | None ->
          find_opt elt right )

  let rec remove elt queue =
    match queue with
    | Empty ->
        queue
    | Node (_, e, _, _) as queue when e = elt ->
        remove_top queue
    | Node (p, e, left, right) ->
        Node (p, e, remove elt left, remove elt right)

  let update_prio elt prio' queue = remove elt queue |> insert prio' elt
end

module Pos = struct
  type t = int * int

  let compare = Stdlib.compare
end

module PosMap = Map.Make (Pos)
module PosSet = Set.Make (Pos)

type map_element = Start | End | Wall | Empty

let read_map_element row col ch =
  let e =
    match ch with
    | 'S' ->
        Start
    | 'E' ->
        End
    | '#' ->
        Wall
    | '.' ->
        Empty
    | _ ->
        failwith "Unknown map element"
  in
  ((row, col), e)

let read_row row s =
  List.mapi (read_map_element row) (String.to_seq s |> List.of_seq)

let read_map path =
  In_channel.with_open_text path In_channel.input_lines
  |> List.mapi read_row |> List.flatten |> PosMap.of_list

let _render_map m =
  let f x y =
    match PosMap.find (x, y) m with
    | Start ->
        "S"
    | End ->
        "E"
    | Wall ->
        "#"
    | Empty ->
        " "
  in
  let (maxr, maxc), _ = PosMap.max_binding m in
  Array.init_matrix (maxc + 1) (maxr + 1) f
  |> Array.map Array.to_list
  |> Array.map (String.concat "")
  |> Array.to_list |> String.concat "\n"

exception No_Path

let is_wall map p =
  match PosMap.find_opt p map with Some e when e = Wall -> true | _ -> false

let find_element map e =
  PosMap.bindings map
  |> List.filter (fun (_, e') -> e = e')
  |> List.map fst |> List.hd

let find_start map = find_element map Start

let find_end map = find_element map End

let neighbours map (r, c) =
  [(r - 1, c); (r, c + 1); (r + 1, c); (r, c - 1)]
  |> List.filter (fun p -> PosMap.mem p map)
  |> List.filter (fun p -> not (is_wall map p))

let rec ucs_aux neighbours goal front expanded prev =
  if PrioQueue.is_empty front then raise No_Path
  else
    let prio, node, front' = PrioQueue.extract_min front in
    if node = goal then prev
    else
      let expanded' = PosSet.add node expanded in
      let front', prev' =
        List.fold_left
          (fun (frnt, prv) n ->
            match PrioQueue.find_opt n frnt with
            | None when PosSet.mem n expanded' ->
                (frnt, prv)
            | None ->
                (PrioQueue.insert (prio + 1) n frnt, PosMap.add n node prv)
            | Some p when p > prio + 1 ->
                (PrioQueue.update_prio n p frnt, PosMap.add n node prv)
            | _ ->
                (frnt, prv) )
          (front', prev) (neighbours node)
      in
      ucs_aux neighbours goal front' expanded' prev'

let ucs neighbours start goal =
  ucs_aux neighbours goal
    PrioQueue.(empty |> insert 0 start)
    PosSet.empty PosMap.empty

let path_of_prev goal prev =
  let rec path_of_prev_aux n path prev =
    match PosMap.find_opt n prev with
    | Some n' ->
        path_of_prev_aux n' (n :: path) prev
    | None ->
        n :: path
  in
  path_of_prev_aux goal [] prev

let gain dt times p1 p2 =
  match (PosMap.find_opt p1 times, PosMap.find_opt p2 times) with
  | Some t1, Some t2 ->
      Some (t2 - t1 - dt)
  | _ ->
      None

let cheat ((r, c) as p) times =
  [(r - 2, c); (r, c + 2); (r + 2, c); (r, c - 2)]
  |> List.map (gain 2 times p)
  |> List.filter_map (function Some t when t > 0 -> Some t | _ -> None)

let collect_cheats path =
  let times = path |> List.mapi (fun t p -> (p, t)) |> PosMap.of_list in
  let rec collect_cheats_aux times acc = function
    | n :: rest ->
        let g = cheat n times in
        collect_cheats_aux times (g :: acc) rest
    | _ ->
        acc |> List.filter (fun g -> not (List.is_empty g))
  in
  collect_cheats_aux times [] path

let map = read_map "./input/day20/input.txt"

(* let times = path |> List.mapi (fun t p -> (p, t)) |> PosMap.of_list *)

let () =
  let path =
    ucs (neighbours map) (find_start map) (find_end map)
    |> path_of_prev (find_end map)
  in
  let cheats = collect_cheats path in
  let cheat_count =
    cheats |> List.flatten |> List.filter (fun t -> t >= 100) |> List.length
  in
  Printf.printf "\nNumber of cheats %d\n%!" cheat_count

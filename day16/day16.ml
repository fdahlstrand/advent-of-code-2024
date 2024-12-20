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

type map_element = Empty | Wall | Start | End

type direction = North | East | South | West

module Pos = struct
  type t = int * int

  let compare (r1, c1) (r2, c2) =
    if Stdlib.compare r1 r2 = 0 then Stdlib.compare c1 c2
    else Stdlib.compare r1 r2
end

module Node = struct
  type t = (int * int) * direction

  let compare = Stdlib.compare
end

module Maze = Map.Make (Pos)
module NodeMap = Map.Make (Node)
module NodeSet = Set.Make (Node)

let read_map path =
  In_channel.with_open_text path In_channel.input_lines
  |> List.mapi (fun row s ->
         List.mapi
           (fun col ch ->
             ( (row, col)
             , match ch with
               | '#' ->
                   Wall
               | '.' ->
                   Empty
               | 'S' ->
                   Start
               | 'E' ->
                   End
               | _ ->
                   failwith ("Unexpected character: '" ^ String.make 1 ch ^ "'")
             ) )
           (String.to_seq s |> List.of_seq) )
  |> List.flatten |> Maze.of_list

let find_in_maze maze element =
  Maze.filter (fun _ e -> e = element) maze
  |> Maze.bindings
  |> List.map (fun (p, _) -> p)
  |> List.hd

let find_start maze = find_in_maze maze Start

let find_end maze = find_in_maze maze End

let move_forward ((row, col), dir, score) =
  let pos' =
    match dir with
    | North ->
        (row - 1, col)
    | East ->
        (row, col + 1)
    | South ->
        (row + 1, col)
    | West ->
        (row, col - 1)
  in
  (pos', dir, score + 1)

let turn_left (pos, dir, score) =
  let dir' =
    match dir with
    | North ->
        West
    | East ->
        North
    | South ->
        East
    | West ->
        South
  in
  (pos, dir', score + 1000)

let turn_right (pos, dir, score) =
  let dir' =
    match dir with
    | North ->
        East
    | East ->
        South
    | South ->
        West
    | West ->
        North
  in
  (pos, dir', score + 1000)

let is_valid_state maze (pos, _, _) =
  match Maze.find_opt pos maze with
  | Some Wall | None ->
      false
  | Some Start | Some End | Some Empty ->
      true

let next_moves maze state =
  [move_forward state; turn_left state; turn_right state]
  |> List.filter (is_valid_state maze)

let string_of_dir = function
  | North ->
      "^"
  | East ->
      ">"
  | South ->
      "v"
  | West ->
      "<"

let rec _print_moves moves =
  match moves with
  | ((row, col), dir, score) :: t ->
      let () =
        Printf.printf "(%d,%d) %s %d\n%!" row col (string_of_dir dir) score
      in
      _print_moves t
  | _ ->
      ()

let visited_score pos maze_score =
  match Maze.find_opt pos maze_score with
  | Some score ->
      score
  | None ->
      Int.max_int

let walk maze =
  let start_pos = find_start maze in
  let end_pos = find_end maze in
  let rec walk' maze next visited =
    match next with
    | (pos, dir, score) :: rest ->
        if pos = end_pos then
          if score < visited_score pos visited then
            let visited' = Maze.update pos (fun _ -> Some score) visited in
            walk' maze rest visited'
          else walk' maze rest visited
        else
          let next' =
            next_moves maze (pos, dir, score)
            |> List.filter (fun (pos, _, score) ->
                   score < visited_score pos visited )
          in
          let visited' = Maze.update pos (fun _ -> Some score) visited in
          walk' maze (next' @ rest) visited'
    | _ ->
        Maze.find end_pos visited
  in
  walk' maze [(start_pos, East, 0)] Maze.empty

let update_neighbours maze q seen ((pos_u, dir_u), dist_u) dist prev =
  let ns =
    next_moves maze (pos_u, dir_u, dist_u)
    |> List.filter (fun (pos, dir, _) -> not (NodeSet.mem (pos, dir) seen))
  in
  List.fold_left
    (fun (q', dist', prev') (pos, dir, alt) ->
      let dist_v =
        NodeMap.find_opt (pos, dir) dist' |> Option.value ~default:Int.max_int
      in
      (*TODO: Handle case when alt = dist_v *)
      if alt < dist_v then
        ( q' (*TODO: Add v to q*)
        , NodeMap.update (pos, dir) (fun _ -> Some alt) dist'
        , NodeMap.update (pos, dir) (fun _ -> Some [(pos_u, dir_u)]) prev' )
      else (q', dist', prev') )
    (q, dist, prev) ns

let dijkstra maze =
  let start_pos = find_start maze in
  let end_pos = find_end maze in
  let rec dijkstra_aux q seen dist prev =
    if PrioQueue.is_empty q then (dist, prev)
    else
      let dist_u, u, q' = PrioQueue.extract_min q in
      (* Don't break when u = end_pos. There might be more paths that are equally good *)
      let q', dist', prev' =
        update_neighbours maze q' seen (u, dist_u) dist prev
      in
      let seen' = NodeSet.add u seen in
      dijkstra_aux q' seen' dist' prev'
  in
  dijkstra_aux
    PrioQueue.(empty |> insert 0 (start_pos, East))
    NodeSet.empty
    NodeMap.(empty |> add (start_pos, East) 0)
    NodeMap.empty

let () =
  Printf.printf "\nLowest score: %d\n%!"
    (walk (read_map "./input/day16/input.txt"))

module Graph = struct
  module Vertex = struct
    type t = string

    let compare = Stdlib.compare
  end

  module VertexSet = Set.Make (Vertex)
  module EdgeMap = Map.Make (Vertex)

  let empty = (VertexSet.empty, EdgeMap.empty)

  let add_edge src tgt (vertices, edges) =
    let add_edge' a b edges =
      EdgeMap.update a
        (function
          | Some vs ->
              Some (VertexSet.add b vs)
          | None ->
              Some VertexSet.(empty |> add b) )
        edges
    in
    let vertices' = VertexSet.(add src vertices |> add tgt) in
    let edges' = edges |> add_edge' src tgt |> add_edge' tgt src in
    (vertices', edges')

  let vertices (v', _) = VertexSet.to_list v'

  let vertices_from (_, edges) v =
    match EdgeMap.find_opt v edges with
    | Some vs ->
        VertexSet.to_list vs
    | None ->
        []
end

let read_network_map path =
  In_channel.with_open_text path In_channel.input_lines
  |> List.map (String.split_on_char '-')
  |> List.map (function [a; b] -> (a, b) | _ -> failwith "Invalid input")
  |> List.fold_left (fun g (a, b) -> Graph.add_edge a b g) Graph.empty

let find_interconnected graph n s =
  let open Graph in
  let rec find_interconnected_aux n visited path paths v =
    if n = 0 && v = s then path :: paths
    else if n = 0 then paths
    else if VertexSet.mem v visited then paths
    else
      let visited' = VertexSet.add v visited in
      vertices_from graph v
      |> List.fold_left
           (find_interconnected_aux (n - 1) visited' (v :: path))
           paths
  in
  find_interconnected_aux n VertexSet.empty [] [] s

let starts_with_t s = String.get s 0 = 't'

let () =
  let graph = read_network_map "./input/day23/input.txt" in
  let count =
    Graph.vertices graph
    |> List.map (find_interconnected graph 3)
    |> List.flatten
    |> List.map (List.sort Stdlib.compare)
    |> List.sort_uniq Stdlib.compare
    |> List.filter (List.exists starts_with_t)
    |> List.length
  in
  Printf.printf "Number of sets: %d\n%!" count

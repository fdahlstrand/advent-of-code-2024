module Pos = struct
  type t = int * int

  let compare = Stdlib.compare
end

module PosMap = Map.Make (Pos)

type item = Key of int | Lock of int

let get_row row map =
  PosMap.filter (fun (r, _) _ -> r = row) map |> PosMap.bindings |> List.map snd

let get_column col map =
  PosMap.filter (fun (_, c) _ -> c = col) map |> PosMap.bindings |> List.map snd

let is_key map = get_row 6 map |> List.for_all (fun x -> x = '#')

let is_lock map = get_row 0 map |> List.for_all (fun x -> x = '#')

let posmap_of_string s =
  String.split_on_char '\n' s
  |> List.mapi (fun row s ->
         List.mapi
           (fun col ch -> ((row, col), ch))
           (s |> String.to_seq |> List.of_seq) )
  |> List.flatten |> PosMap.of_list

let height col map =
  List.map (fun e -> if e = '#' then 1 else 0) (get_column col map)
  |> List.fold_left ( + ) (-1)

let heights map = List.init 5 (fun c -> height c map)

let encode_lock map =
  heights map
  |> List.mapi (fun n h -> ((1 lsl h) - 1) lsl (n * 5))
  |> List.fold_left ( lor ) 0

let encode_key map =
  heights map
  |> List.mapi (fun n h -> (((1 lsl h) - 1) lsl (5 - h)) lsl (5 * n))
  |> List.fold_left ( lor ) 0

let item_of_string s =
  let map = posmap_of_string s in
  if is_key map then Key (encode_key map)
  else if is_lock map then Lock (encode_lock map)
  else failwith "Invalid scematic"

let fit pair =
  match pair with Key x, Lock y | Lock y, Key x -> x land y = 0 | _ -> false

let read_schematics path =
  In_channel.with_open_text path In_channel.input_all
  |> Str.split (Str.regexp "\n\n")
  |> List.map String.trim |> List.map item_of_string

let () =
  let items = read_schematics "./input/day25/input.txt" in
  let locks, keys =
    List.partition (function Lock _ -> true | Key _ -> false) items
  in
  let pairs =
    List.map (fun l -> List.map (fun k -> (l, k)) keys) locks |> List.flatten
  in
  let count = List.map fit pairs |> List.filter (fun x -> x) |> List.length in
  Printf.printf "Number of fitting key/lock pairs %d\n%!" count

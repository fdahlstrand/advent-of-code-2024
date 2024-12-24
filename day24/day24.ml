module Circuit = struct
  type t =
    | Label of string
    | Xor of t * t
    | Or of t * t
    | And of t * t
    | Input of int
end

module LabelMap = Map.Make (String)

let parse_connection s =
  let open Circuit in
  let rexp =
    Str.regexp
      {|\([a-z0-9][a-z0-9][a-z0-9]\) \(AND\|OR\|XOR\) \([a-z0-9][a-z0-9][a-z0-9]\) -> \([a-z0-9][a-z0-9][a-z0-9]\)|}
  in
  if Str.string_match rexp s 0 then
    let out = Str.matched_group 4 s in
    let gate = Str.matched_group 2 s in
    let in1 = Str.matched_group 1 s in
    let in2 = Str.matched_group 3 s in
    match gate with
    | "XOR" ->
        (out, Xor (Label in1, Label in2))
    | "OR" ->
        (out, Or (Label in1, Label in2))
    | "AND" ->
        (out, And (Label in1, Label in2))
    | _ ->
        failwith ("Unsupported circuit type: " ^ gate)
  else failwith ("Invalid input: " ^ s)

let parse_input s =
  let open Circuit in
  let rexp = Str.regexp {|\([a-z0-9][a-z0-9][a-z0-9]\): \(0\|1\)|} in
  if Str.string_match rexp s 0 then
    let out = Str.matched_group 1 s in
    let value =
      match Str.matched_group 2 s with
      | "0" ->
          0
      | "1" ->
          1
      | s ->
          failwith ("Unexpected value " ^ s)
    in
    (out, Input value)
  else failwith ("Invalid input: " ^ s)

let outputs = LabelMap.filter (fun k _ -> String.starts_with ~prefix:"z" k)

let rec evaluate labels circuit =
  let open Circuit in
  let evaluate = evaluate labels in
  match circuit with
  | Xor (left, right) ->
      evaluate left lxor evaluate right
  | Or (left, right) ->
      evaluate left lor evaluate right
  | And (left, right) ->
      evaluate left land evaluate right
  | Input value ->
      value
  | Label label ->
      evaluate (LabelMap.find label labels)

let read_connections path =
  In_channel.with_open_text path In_channel.input_all
  |> Str.split (Str.regexp "\n\n")
  |> function
  | [a; b] ->
      LabelMap.union
        (fun _ _ _ -> failwith "Short cicuit")
        ( a |> String.trim |> String.split_on_char '\n' |> List.map String.trim
        |> List.map parse_input |> LabelMap.of_list )
        ( b |> String.trim |> String.split_on_char '\n' |> List.map String.trim
        |> List.map parse_connection |> LabelMap.of_list )
  | _ ->
      failwith "Invalid input"

let () =
  let labels = read_connections "./input/day24/input.txt" in
  let out =
    outputs labels |> LabelMap.bindings
    |> List.map (fun (_, g) -> evaluate labels g)
    |> List.mapi (fun n v -> v lsl n)
    |> List.fold_left ( + ) 0
  in
  Printf.printf "Output: %d\n" out

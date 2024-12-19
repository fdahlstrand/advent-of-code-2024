let read_input path =
  let data = In_channel.with_open_text path In_channel.input_all in
  match Str.split (Str.regexp "\n\n") data with
  | [a; b] ->
      ( Str.split (Str.regexp ", ") a
      , String.split_on_char '\n' b |> List.map String.trim )
  | _ ->
      failwith "Invalid input"

let starts_with s prefix =
  let np = String.length prefix in
  let ns = String.length s in
  ns >= np && Str.string_before s np = prefix

let remove_prefix s prefix =
  let np = String.length prefix in
  let ns = String.length s in
  String.sub s np (ns - np)

let count_possible_designs patterns design =
  let cache = Hashtbl.create 10 in
  let rec foo_aux s =
    try Hashtbl.find cache s
    with Not_found ->
      if s = "" then 1
      else
        let possibilites = List.filter (starts_with s) patterns in
        let acc' =
          List.fold_left
            (fun acc p -> acc + foo_aux (remove_prefix s p))
            0 possibilites
        in
        Hashtbl.add cache s acc' ; acc'
  in
  foo_aux design

let () =
  let patterns, designs = read_input "./input/day19/input.txt" in
  let possible =
    List.map (fun d -> count_possible_designs patterns d) designs
    |> List.filter (fun n -> n > 0)
    |> List.length
  in
  Printf.printf "\nPossible designs: %d\n%!" possible

let () =
  let patterns, designs = read_input "./input/day19/input.txt" in
  let all_designs =
    List.map (fun d -> count_possible_designs patterns d) designs
    |> List.fold_left ( + ) 0
  in
  Printf.printf "Possible designs: %d\n%!" all_designs

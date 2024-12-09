type disk_content_type = Empty of int | File of (int * int)

let _first_sample = "12345"

let _second_sample = "2333133121414131402"

let read_disk_map s =
  let rec read_disk_map' is_space acc id = function
    | ch :: rest ->
        let len = int_of_char ch - int_of_char '0' in
        if is_space then
          if len > 0 then
            read_disk_map' (not is_space) (Empty len :: acc) id rest
          else read_disk_map' (not is_space) acc id rest
        else read_disk_map' (not is_space) (File (len, id) :: acc) (id + 1) rest
    | [] ->
        acc
  in
  String.to_seq s |> List.of_seq |> read_disk_map' false [] 0 |> List.rev

let collect_files fs =
  List.filter (fun e -> match e with File _ -> true | _ -> false) fs
  |> List.rev

let remove_file_by_id id files =
  List.filter
    (fun f -> match f with Empty _ -> true | File (_, x) -> x != id)
    files

let rec compact_aux fs acc to_process =
  match fs with
  | File (file_len, id) :: rest -> (
    match to_process with
    | File (file_len', id') :: rest_to_process ->
        if id' = id then
          compact_aux rest (File (file_len', id) :: acc) rest_to_process
        else
          compact_aux rest
            (File (file_len, id) :: acc)
            (remove_file_by_id id to_process)
    | [] ->
        compact_aux rest (Empty file_len :: acc) to_process
    | Empty _ :: _ ->
        failwith "Internal Error: Only files should be processed" )
  | Empty space_len :: rest -> (
    match to_process with
    | File (file_len, id) :: rest_to_process ->
        if file_len = space_len then
          compact_aux rest (File (file_len, id) :: acc) rest_to_process
        else if file_len < space_len then
          compact_aux
            (Empty (space_len - file_len) :: rest)
            (File (file_len, id) :: acc)
            rest_to_process
        else
          compact_aux rest
            (File (space_len, id) :: acc)
            (File (file_len - space_len, id) :: rest_to_process)
    | Empty _ :: _ ->
        failwith "Internal Error: Only files should be processed"
    | [] ->
        compact_aux rest (Empty space_len :: acc) to_process )
  | [] ->
      acc

let compact fs = compact_aux fs [] (collect_files fs) |> List.rev

let rec checksum_elt pos acc = function
  | File (len, id) ->
      if len > 0 then
        checksum_elt (pos + 1) (acc + (id * pos)) (File (len - 1, id))
      else acc
  | Empty _ ->
      0

let checksum fs =
  let rec checksum' pos acc = function
    | File (len, id) :: t ->
        checksum' (pos + len) (acc + checksum_elt pos 0 (File (len, id))) t
    | Empty len :: t ->
        checksum' (pos + len) acc t
    | [] ->
        acc
  in
  checksum' 0 0 fs

let s =
  In_channel.with_open_text "./input/day09/input.txt" In_channel.input_all
  |> String.trim

let () =
  Printf.printf "\nFilesystem checksum: %d\n"
    (read_disk_map s |> compact |> checksum)

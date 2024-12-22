let next secret =
  let prune = (1 lsl 24) - 1 in
  let a = secret lxor (secret lsl 6) land prune in
  let b = a lxor (a lsr 5) land prune in
  b lxor (b lsl 11) land prune

let rec calculate n secret =
  if n = 0 then secret else calculate (n - 1) (next secret)

let read_initial_secrets path =
  In_channel.with_open_text path In_channel.input_lines
  |> List.map int_of_string

let () =
  let secret_sum =
    read_initial_secrets "./input/day22/input.txt"
    |> List.map (calculate 2000)
    |> List.fold_left ( + ) 0
  in
  Printf.printf "Sum of secrets: %d\n%!" secret_sum

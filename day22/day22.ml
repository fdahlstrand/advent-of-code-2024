module Sequence = struct
  type t = int * int * int * int

  let compare = Stdlib.compare
end

module SequenceMap = Map.Make (Sequence)

let next secret =
  let prune = (1 lsl 24) - 1 in
  let a = secret lxor (secret lsl 6) land prune in
  let b = a lxor (a lsr 5) land prune in
  b lxor (b lsl 11) land prune

let rec calculate n secret =
  if n = 0 then secret else calculate (n - 1) (next secret)

let calculate_all n secret =
  let rec calculate_all_aux n secret acc =
    if n = 0 then secret :: acc
    else calculate_all_aux (n - 1) (next secret) (secret :: acc)
  in
  calculate_all_aux n secret [] |> List.rev

let diff lst =
  let rec diff_aux acc = function
    | a :: b :: rest ->
        diff_aux ((b - a) :: acc) (b :: rest)
    | _ ->
        acc |> List.rev
  in
  diff_aux [] lst

let all_sequences secret =
  let prices = calculate_all 1999 secret |> List.map (fun x -> x mod 10) in
  let price_change = List.combine prices (0 :: diff prices) |> List.tl in
  let rec all_sequences_aux acc = function
    | (_, da) :: ((_, db) as b) :: ((_, dc) as c) :: ((price, dd) as d) :: rest
      ->
        let sequence = (da, db, dc, dd) in
        if SequenceMap.mem sequence acc then
          all_sequences_aux acc (b :: c :: d :: rest)
        else
          let acc' = SequenceMap.add sequence price acc in
          all_sequences_aux acc' (b :: c :: d :: rest)
    | _ ->
        acc
  in
  all_sequences_aux SequenceMap.empty price_change

let merge = SequenceMap.union (fun _ pa pb -> Some (pa + pb))

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

let () =
  let all_sequences =
    read_initial_secrets "./input/day22/input.txt"
    |> List.map all_sequences
    |> List.fold_left merge SequenceMap.empty
  in
  let _best_seq, best_price =
    SequenceMap.fold
      (fun seq price (best_seq, best_price) ->
        if price > best_price then (seq, price) else (best_seq, best_price) )
      all_sequences
      ((0, 0, 0, 0), 0)
  in
  Printf.printf "Best price: %d\n%!" best_price

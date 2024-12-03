type instr = Mul of int * int | Do | Dont

let input_path = "./input/day03/input.txt"

let mulop_regex =
  Str.regexp {|\(mul\)(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|}

let do_regex = Str.regexp {|\(do\)()|}

let dont_regex = Str.regexp {|\(don't\)()|}

let read_memory path = In_channel.with_open_text path In_channel.input_all

let rec match_any s n = function
  | r :: t ->
      if Str.string_match r s n then
        let op = Str.matched_group 1 s in
        match op with
        | "mul" ->
            let a = Str.matched_group 2 s in
            let b = Str.matched_group 3 s in
            Some (Mul (int_of_string a, int_of_string b), Str.match_end ())
        | "do" ->
            Some (Do, Str.match_end ())
        | "don't" ->
            Some (Dont, Str.match_end ())
        | _ ->
            failwith ("Unknown opcode '" ^ op ^ "'")
      else match_any s n t
  | [] ->
      None

let analyze_memory mem =
  let rec analyze_memory2 matches mem offset =
    if offset < String.length mem then
      match match_any mem offset [mulop_regex; do_regex; dont_regex] with
      | Some (m, n) ->
          analyze_memory2 (matches @ [m]) mem n
      | None ->
          analyze_memory2 matches mem (offset + 1)
    else matches
  in
  analyze_memory2 [] mem 0

let run_v1 prog =
  let rec run2 acc = function
    | i :: t -> (
      match i with
      | Mul (a, b) ->
          run2 (acc + (a * b)) t
      | Do ->
          run2 acc t
      | Dont ->
          run2 acc t )
    | [] ->
        acc
  in
  run2 0 prog

let run_v2 prog =
  let rec run2 acc enabled = function
    | i :: t -> (
      match i with
      | Mul (a, b) ->
          if enabled then run2 (acc + (a * b)) enabled t else run2 acc enabled t
      | Do ->
          run2 acc true t
      | Dont ->
          run2 acc false t )
    | [] ->
        acc
  in
  run2 0 true prog

let program = read_memory input_path |> analyze_memory

let () =
  Printf.printf "\nResult #1: %d\nResult #2: %d\n" (run_v1 program)
    (run_v2 program)

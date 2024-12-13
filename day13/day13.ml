module PushSet = Set.Make (struct
  type t = int * int

  let compare (a1, b1) (a2, b2) =
    if Stdlib.compare a1 a2 = 0 then Stdlib.compare b1 b2
    else Stdlib.compare a1 a2
end)

let int_of_ceil_float v = int_of_float (ceil v)

let int_of_floor_float v = int_of_float (floor v)

let ext_euclid a b =
  let rec ext_euclid_aux r r' s s' t t' =
    if r' = 0 then (r, (s, t))
    else
      let q = r / r' in
      ext_euclid_aux r' (r - (q * r')) s' (s - (q * s')) t' (t - (q * t'))
  in
  ext_euclid_aux a b 1 0 0 1

let rec eval x y acc (s, e) =
  if s > e then List.rev acc else eval x y ((x s, y s) :: acc) (s + 1, e)

let calculate a b c =
  let gcd, (s, t) = ext_euclid a b in
  let k = c / gcd in
  let x r = (k * s) - (b / gcd * r) in
  let y r = (k * t) + (a / gcd * r) in
  let x_v v = float_of_int (((k * s) - v) * gcd) /. float_of_int b in
  let y_v v = float_of_int ((v - (k * t)) * gcd) /. float_of_int a in
  let x_rrng = (int_of_ceil_float (x_v 100), int_of_floor_float (x_v 0)) in
  let y_rrng = (int_of_ceil_float (y_v 0), int_of_floor_float (y_v 100)) in
  let range = (max (fst x_rrng) (fst y_rrng), min (snd x_rrng) (snd y_rrng)) in
  (* (x, y, x_rrng, y_rrng, range, eval x y [] range) *)
  eval x y [] range

let check_machine adjust ((ax, ay), (bx, by), (px, py)) =
  let push_x = calculate ax bx (px + adjust) |> PushSet.of_list in
  let push_y = calculate ay by (py + adjust) |> PushSet.of_list in
  PushSet.inter push_x push_y |> PushSet.to_list

let parse_machine = function
  | [a; b; p] ->
      let button_a = Str.regexp {|Button A: X\+\([0-9]+\), Y\+\([0-9]+\)|} in
      let button_b = Str.regexp {|Button B: X\+\([0-9]+\), Y\+\([0-9]+\)|} in
      let price = Str.regexp {|Prize: X=\([0-9]+\), Y=\([0-9]+\)|} in
      let get_numbers rexp s =
        if Str.string_match rexp s 0 then
          ( int_of_string (Str.matched_group 1 s)
          , int_of_string (Str.matched_group 2 s) )
        else failwith ("Parse error: " ^ s)
      in
      (get_numbers button_a a, get_numbers button_b b, get_numbers price p)
  | _ ->
      failwith "Invalid input"

let read_machines path =
  In_channel.with_open_text path In_channel.input_all
  |> Str.split (Str.regexp "\n\n")
  |> List.map String.trim
  |> List.map (Str.split (Str.regexp "\n"))
  |> List.map parse_machine

let tokens =
  read_machines "./input/day13/input.txt"
  |> List.map (check_machine 0)
  |> List.map (List.map (fun (a, b) -> (3 * a) + b))
  |> List.filter (fun l -> not (List.is_empty l))
  |> List.map (List.sort Stdlib.compare)
  |> List.map List.hd |> List.fold_left ( + ) 0

let adjusted_tokens =
  read_machines "./input/day13/sample.txt"
  |> List.map (check_machine 10000000000000)
  |> List.map (List.map (fun (a, b) -> (3 * a) + b))
  |> List.filter (fun l -> not (List.is_empty l))
  |> List.map (List.sort Stdlib.compare)
  |> List.map List.hd |> List.fold_left ( + ) 0

let () =
  Printf.printf "\nMinimum tokens: %d\nAdjusted minimum tokens %d\n" tokens
    adjusted_tokens

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

let labels = read_connections "./input/day24/input.txt"

let rec _expand labels output =
  let open Circuit in
  let expand = _expand labels in
  match LabelMap.find output labels with
  | Xor (Label left, Label right) ->
      Xor (expand left, expand right)
  | Or (Label left, Label right) ->
      Or (expand left, expand right)
  | And (Label left, Label right) ->
      And (expand left, expand right)
  | Input _ ->
      Label output
  | Label label ->
      Label label
  | _ ->
      failwith "That was unexpected!"

let rec _expand_str labels output =
  let open Circuit in
  let expand_str = _expand_str labels in
  match LabelMap.find output labels with
  | Xor (Label left, Label right) ->
      Printf.sprintf "[%s](%s ^ %s)" output (expand_str left) (expand_str right)
  | Or (Label left, Label right) ->
      Printf.sprintf "[%s](%s | %s)" output (expand_str left) (expand_str right)
  | And (Label left, Label right) ->
      Printf.sprintf "[%s](%s & %s)" output (expand_str left) (expand_str right)
  | Input _ ->
      output
  | _ ->
      failwith "That was unexpected!"

let rec are_equal expr1 expr2 =
  let open Circuit in
  match (expr1, expr2) with
  | Xor (left1, right1), Xor (left2, right2) ->
      (are_equal left1 left2 && are_equal right1 right2)
      || (are_equal left1 right2 && are_equal right1 left2)
  | Or (left1, right1), Or (left2, right2) ->
      (are_equal left1 left2 && are_equal right1 right2)
      || (are_equal left1 right2 && are_equal right1 left2)
  | And (left1, right1), And (left2, right2) ->
      (are_equal left1 left2 && are_equal right1 right2)
      || (are_equal left1 right2 && are_equal right1 left2)
  | Input label1, Input label2 ->
      label1 = label2
  | Label label1, Label label2 ->
      label1 = label2
  | _ ->
      false

let find_expr labels expr =
  LabelMap.filter (fun _ e -> are_equal e expr) labels
  |> LabelMap.bindings
  |> (fun lst -> List.nth_opt lst 0)
  |> Option.map fst

let sum_term labels output list =
  let open Circuit in
  let open Printf in
  let i = int_of_string (String.sub output 1 2) in
  let xi = sprintf "x%02d" i in
  let yi = sprintf "y%02d" i in
  let expr = Xor (Label xi, Label yi) in
  (sprintf "S%02d" i, find_expr labels expr) :: list

let carry1_term labels output list =
  let open Circuit in
  let open Printf in
  let i = int_of_string (String.sub output 1 2) in
  let xi = sprintf "x%02d" i in
  let yi = sprintf "y%02d" i in
  let expr = And (Label xi, Label yi) in
  (sprintf "c'%02d" i, find_expr labels expr) :: list

let rec carry2_term labels n prev_carry list =
  let open Circuit in
  let open Printf in
  let option_to_string = function Some o -> o | None -> "[None]" in
  if n = 45 then list
  else
    let s = List.assoc (sprintf "S%02d" n) list in
    let c' = List.assoc (sprintf "c'%02d" n) list in
    let c'' =
      match (s, prev_carry) with
      | Some l1, Some l2 ->
          find_expr labels (And (Label l1, Label l2))
      | _ ->
          None
    in
    let c =
      match (c', c'') with
      | Some l1, Some l2 ->
          find_expr labels (Or (Label l1, Label l2))
      | _ ->
          None
    in
    let () =
      printf ">> %d: S=%s, c=%s, c'=%s, c''=%s\n%!" n (option_to_string s)
        (option_to_string c) (option_to_string c') (option_to_string c'')
    in
    carry2_term labels (n + 1) c
      ((sprintf "c''%02d" n, c'') :: (sprintf "c%02d" n, c) :: list)

(*
  S  (i) = x(i) ^ y(i)
  c  (i) = c'(i) | c''(i)
  c' (i) = x(i) & y(i)
  c''(i) = S(i) & c(i-1)
*)
let () =
  let stat = [] in
  let stat =
    outputs labels |> LabelMap.bindings |> List.map fst
    |> List.fold_left (fun l e -> sum_term labels e l) stat
  in
  let stat =
    outputs labels |> LabelMap.bindings |> List.map fst
    |> List.fold_left (fun l e -> carry1_term labels e l) stat
  in
  let init_carry =
    find_expr labels (Circuit.And (Circuit.Label "x00", Circuit.Label "y00"))
  in
  ignore (carry2_term labels 1 init_carry stat)

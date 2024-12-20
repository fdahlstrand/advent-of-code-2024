exception CPU_Halt

type cpu = {a: int; b: int; c: int; pc: int}

type instruction =
  | Adv of int
  | Bxl of int
  | Bst of int
  | Jnz of int
  | Bxc
  | Out of int
  | Bdv of int
  | Cdv of int

module Memory = Map.Make (Int)

let read_debugger_input path =
  let parse_registers s =
    String.split_on_char '\n' s
    |> List.map (fun s ->
           if
             Str.string_match
               (Str.regexp {|Register \([ABC]\): \([0-9]+\)|})
               s 0
           then (Str.matched_group 1 s, int_of_string (Str.matched_group 2 s))
           else failwith "Bad register definition" )
  in
  let parse_program s =
    Str.split (Str.regexp {|Program: |}) s
    |> List.hd |> String.trim |> String.split_on_char ','
    |> List.map int_of_string
  in
  In_channel.with_open_text path In_channel.input_all
  |> Str.split (Str.regexp "\n\n")
  |> function
  | [a; b] ->
      (parse_registers a, parse_program b)
  | _ ->
      failwith "Invalid input"

let make_cpu (regs, mem) =
  ( { a= List.assoc "A" regs
    ; b= List.assoc "B" regs
    ; c= List.assoc "C" regs
    ; pc= 0 }
  , List.mapi (fun addr data -> (addr, data)) mem |> Memory.of_list )

let read_instr addr mem =
  let opcode = Memory.find addr mem in
  let operand = Memory.find (addr + 1) mem in
  match opcode with
  | 0 ->
      Adv operand
  | 1 ->
      Bxl operand
  | 2 ->
      Bst operand
  | 3 ->
      Jnz operand
  | 4 ->
      Bxc
  | 5 ->
      Out operand
  | 6 ->
      Bdv operand
  | 7 ->
      Cdv operand
  | _ ->
      failwith "Unknown opcode"

let combo {a; b; c; _} operand =
  match operand with
  | n when 0 <= n && n <= 3 ->
      n
  | 4 ->
      a
  | 5 ->
      b
  | 6 ->
      c
  | _ ->
      failwith "Bad combo operand"

let step (({a; b; c; pc} as cpu), mem, out) =
  if Memory.mem pc mem then
    let instr = read_instr pc mem in
    match instr with
    | Adv op ->
        let v = combo cpu op in
        ({cpu with a= a / (1 lsl v); pc= pc + 2}, mem, out)
    | Bxl op ->
        ({cpu with b= b lxor op; pc= pc + 2}, mem, out)
    | Bst op ->
        let v = combo cpu op in
        ({cpu with b= v mod 8; pc= pc + 2}, mem, out)
    | Jnz op ->
        if a = 0 then ({cpu with pc= pc + 2}, mem, out)
        else ({cpu with pc= op}, mem, out)
    | Bxc ->
        ({cpu with b= b lxor c; pc= pc + 2}, mem, out)
    | Out op ->
        let v = combo cpu op mod 8 in
        ({cpu with pc= pc + 2}, mem, v :: out)
    | Bdv op ->
        let v = combo cpu op in
        ({cpu with b= a / (1 lsl v); pc= pc + 2}, mem, out)
    | Cdv op ->
        let v = combo cpu op in
        ({cpu with c= a / (1 lsl v); pc= pc + 2}, mem, out)
  else raise CPU_Halt

let run (cpu, mem) =
  let rec run' state =
    try
      let state' = step state in
      run' state'
    with CPU_Halt -> state
  in
  let _, _, out = run' (cpu, mem, []) in
  out |> List.rev

let step_size digit = 1 lsl (3 * digit)

let options cpu mem digit offset target =
  [0; 1; 2; 3; 4; 5; 6; 7]
  |> List.map (fun n ->
         run ({cpu with a= offset + (n * step_size digit)}, mem)
         |> fun lst -> List.nth_opt lst digit )
  |> List.mapi (fun n v -> (n, v))
  |> List.filter_map (fun (n, v) ->
         match v with Some x when x = target -> Some n | _ -> None )

let find (cpu, mem) =
  let targets = Memory.bindings mem |> List.map (fun (_, d) -> d) |> List.rev in
  let digits = List.length targets - 1 in
  let rec find_aux digit offset targets =
    match targets with
    | target :: rest ->
        options cpu mem digit offset target
        |> List.map (fun n ->
               find_aux (digit - 1) (offset + (n * step_size digit)) rest )
        |> List.fold_left min Int.max_int
    | [] ->
        offset
  in
  find_aux digits (step_size digits) targets

let () =
  Printf.printf "Program Output: %s\n%!"
    ( read_debugger_input "./input/day17/input.txt"
    |> make_cpu |> run |> List.map string_of_int |> String.concat "," )

let () =
  Printf.printf "Register A value: %d\n%!"
    (read_debugger_input "./input/day17/input.txt" |> make_cpu |> find)

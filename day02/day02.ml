let input_path = "./input/day02/input.txt"

let whitespace = Str.regexp {|[ \t]+|}

let report_of_string s = Str.split whitespace s |> List.map int_of_string

let read_reports path =
  In_channel.with_open_text path In_channel.input_lines
  |> List.map report_of_string

let rec difference = function
  | a :: b :: t ->
      (b - a) :: difference (b :: t)
  | _ ->
      []

let rec dampen d p lst =
  match lst with h :: t -> dampen (d @ [p @ t]) (p @ [h]) t | [] -> d

let is_in_range x = (1 <= x && x <= 3) || (-3 <= x && x <= -1)

let is_increasing x = x > 0

let is_decreasing x = x < 0

let is_safe report =
  (List.for_all is_increasing report || List.for_all is_decreasing report)
  && List.for_all is_in_range report

let is_safe_dampened report =
  dampen [report] [] report |> List.map difference |> List.map is_safe
  |> List.exists (fun x -> x)

let reports = read_reports input_path

let nbr_of_safe_reports =
  List.map difference reports
  |> List.map is_safe
  |> List.filter (fun x -> x)
  |> List.length

let nbr_of_safe_dampened_reports =
  List.map is_safe_dampened reports |> List.filter (fun x -> x) |> List.length

let () =
  Printf.printf
    "\nNumber of safe reports: %d\nNumber of safe dampened reports: %d\n"
    nbr_of_safe_reports nbr_of_safe_dampened_reports

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

let is_safe report =
  let diff = difference report in
  ( List.for_all (fun delta -> delta > 0) diff
  || List.for_all (fun delta -> delta < 0) diff )
  && List.for_all is_in_range diff

let is_safe_dampened report = dampen [report] [] report |> List.exists is_safe

let count_safe f reports = List.filter f reports |> List.length

let reports = read_reports input_path

let nbr_of_safe_reports = count_safe is_safe reports

let nbr_of_safe_dampened_reports = count_safe is_safe_dampened reports

let () =
  Printf.printf
    "\nNumber of safe reports: %d\nNumber of safe dampened reports: %d\n"
    nbr_of_safe_reports nbr_of_safe_dampened_reports

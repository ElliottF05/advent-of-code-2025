let file = "./bin/day5/part1.txt"

let rec get_file_section (ic : in_channel) : string list = 
  try
    let line = input_line ic in
    match line with
    | "" -> []
    | _ -> List.append [line] (get_file_section ic)
  with End_of_file ->
    []

let parse_range (line : string) : int * int = 
  let parts = String.split_on_char '-' line
  |> List.map int_of_string
  in
  (List.nth parts 0, List.nth parts 1)

let ranges_contain_id (ranges : (int * int) list) (id : int) : bool = 
  List.exists (fun (low, high) -> low <= id && id <= high) ranges

let main () : string = 
  let ic = open_in file in

  let ranges = get_file_section ic 
  |> List.map parse_range
  in

  let ids = get_file_section ic
  |> List.map int_of_string
  in

  let count = ids
  |> List.filter (ranges_contain_id ranges)
  |> List.length
  in

  string_of_int count


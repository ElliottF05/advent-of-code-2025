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

let main () : string = 
  let ic = open_in file in

  let (result, _) = get_file_section ic 
  |> List.map parse_range
  |> List.sort (fun (l1,_r1) (l2,_r2) -> l1 - l2)
  |> List.fold_left (fun (count, prev_end) (l,r) -> 
      let start = max l (prev_end + 1) in
      let added = max 0 (r - start + 1) in
      (count + added, max prev_end r)
    )
    (0, -1)
  in

  string_of_int result
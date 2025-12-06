let file = "./bin/day6/part1.txt"

let rec read_file ic = 
  try
    let line = input_line ic in
    match line with
    | "" -> []
    | _ -> line :: read_file ic
  with End_of_file ->
    []

let split_input_to_nums_ops lines = 
  let length = List.length lines in
  (List.take (length - 1) lines, List.nth lines (length - 1))

let map_ops_line ops_line = 
  ops_line
  |> String.split_on_char ' '
  |> List.filter (fun s -> s = "*" || s = "+")

let map_nums_line nums_line =
  nums_line 
  |> String.to_seq
  |> Seq.map (fun c ->
    match c with
    | ' ' -> None
    | _ -> Some (int_of_char c - int_of_char '0')
  )
  |> Array.of_seq

let map_nums_lines nums_lines = 
  nums_lines
  |> List.map map_nums_line
  |> Array.of_list

let digits_to_num digits = 
  List.fold_left (fun acc d -> 10 * acc + d) 0 digits

let read_nums_col nums c = 
  let num_rows = Array.length nums in
  List.of_seq (Seq.filter_map (fun r -> nums.(r).(c)) (Utils.range 0 num_rows))

let create_nums_groups nums = 
  let num_cols = Array.length (nums.(0)) in

  let groups, last_group = Utils.range 0 num_cols
  |> List.of_seq 
  |> List.fold_left (fun (groups, curr_group) c ->
      match read_nums_col nums c with
      | [] -> (curr_group :: groups, [])
      | digits -> 
        let num = digits_to_num digits in
        (groups, num :: curr_group)
    ) ([], [])
  in

  List.rev (last_group :: groups)

let compute_single nums op_string = 
  let init = if op_string = "*" then 1 else 0 in
  let op = if op_string = "*" then Int.mul else Int.add in
  List.fold_left op init nums

let compute_all num_groups ops = 
  List.map2 compute_single num_groups ops


let main () : string = 
  let ic = open_in file in
  let lines = read_file ic in

  let nums_lines, ops_line = split_input_to_nums_ops lines in

  let ops = map_ops_line ops_line in
  let nums = map_nums_lines nums_lines in

  let nums_groups = create_nums_groups nums in

  let results = compute_all nums_groups ops in

  let final_result = List.fold_left ( + ) 0 results in

  string_of_int final_result
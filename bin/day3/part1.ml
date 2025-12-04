let file = "./bin/day3/part1.txt"

let parse_line (line : string) : int list = 
  line |> String.to_seq |> Seq.map (fun c -> int_of_char c - int_of_char '0') |> List.of_seq

let solve_line (joltages : int list) : int = 
  let _, result = List.fold_left (fun (prev, result) curr -> (max prev curr, max result (10 * prev + curr))) (0, 0) joltages in
  result

let main () : string = 
  let ic = open_in file in
  
  let rec loop () : int = 
    try
      let line = input_line ic in
      let joltages = parse_line line in
      solve_line joltages + loop ()
    with End_of_file ->
      close_in ic;
      0
  in

  string_of_int (loop ())
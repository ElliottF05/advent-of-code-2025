let file = "./bin/day3/part1.txt"

let parse_line (line : string) : int list = 
  line |> String.to_seq |> Seq.map (fun c -> int_of_char c - int_of_char '0') |> List.of_seq

let solve_line (joltages : int list) : int = 

  let rec update_state (state : int list) (prev : int) (curr : int) : int list = 
    let new_state = match state with 
    | [] -> []
    | hd :: tl -> (max hd (10 * prev + curr)) :: update_state tl hd curr
    in

    new_state
  in

  let initial_state = List.init 12 (fun _ -> 0) in
  let _, result = List.fold_left 
    (
      fun (state, result) curr -> 
        let new_state = update_state state 0 curr in
        (new_state, max result (List.fold_left max 0 new_state)
      )
    ) 
    (initial_state, 0) 
    joltages 
  in

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
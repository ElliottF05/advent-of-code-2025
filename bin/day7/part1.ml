let file = "./bin/day7/part1.txt"

module IntSet = Set.Make(Int)

let parse_initial_state line = 
  [String.index line 'S']

let get_next_state state line = 
  let count, new_state = List.fold_left (fun (count, new_state) i ->
      match line.[i] with
      | '^' -> (count + 1, i-1 :: i+1 :: new_state)
      | _ -> (count, i :: new_state)
    ) (0, []) state in
  count, (IntSet.to_list (IntSet.of_list new_state))

let rec loop ic state = 
  try
    let line = input_line ic in
    match state with
    | [] -> loop ic (parse_initial_state line)
    | _ -> let count, new_state = get_next_state state line in
      count + loop ic new_state
  with End_of_file ->
    0

let main () : string = 
  let ic = open_in file in
  let result = loop ic [] in
  close_in ic;

  string_of_int result
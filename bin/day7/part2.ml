let file = "./bin/day7/part1.txt"

let parse_initial_state line = 
  let state = Hashtbl.create 256 in
  Hashtbl.add state (String.index line 'S') 1;
  state

let add_to_table i x table = 
  let prev_x = match Hashtbl.find_opt table i with
  | Some prev_x -> prev_x
  | None -> 0
  in
  Hashtbl.replace table i (prev_x + x)

let get_next_state state line = 
  let new_state = Hashtbl.create 256 in
  Hashtbl.iter (fun i count -> 
      match line.[i] with 
      | '^' -> 
        add_to_table (i-1) count new_state;
        add_to_table (i+1) count new_state;
      | _ -> add_to_table i count new_state;
    ) state;
  new_state

let rec loop ic state = 
  try
    let line = input_line ic in
    let new_state = match Hashtbl.length state with
    | 0 -> parse_initial_state line
    | _ -> get_next_state state line
    in
    loop ic new_state
  with End_of_file ->
    state

let main () : string = 
  let ic = open_in file in
  let final_state = loop ic (Hashtbl.create 256) in
  let result = Hashtbl.fold (fun _i count acc -> acc + count) final_state 0 in
  close_in ic;

  string_of_int result
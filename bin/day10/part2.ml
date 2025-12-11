open Core

let file = "./bin/day10/part1.txt"

let parse_joltage_str joltage_str = 
  joltage_str
  |> String.strip ~drop:(fun c -> Char.(=) c '{' || Char.(=) c '}')
  |> String.split ~on:','
  |> List.map ~f:Int.of_string
  |> List.to_array

let parse_button_str button_str = 
  button_str
  |> String.to_list
  |> List.filter ~f:Char.is_digit
  |> List.map ~f:(fun c -> (Char.to_int c) - (Char.to_int '0'))
  |> List.to_array

let parse_line line = 
  match line |> String.split ~on:' ' |> List.rev with
  | joltage_str :: remaining -> 
    let joltages = parse_joltage_str joltage_str in
    let buttons = remaining
    |> List.take_while ~f:(fun s -> String.contains s '(')
    |> List.map ~f:parse_button_str
    |> List.to_array
    |> Array.sorted_copy ~compare:(fun a b -> Int.compare (Array.length b) (Array.length a))
    in
    joltages, buttons
  | _ -> failwith "invalid line to parse"

let reject remaining best_s curr_s = 
  if Array.exists remaining ~f:(fun x -> x < 0) then 
    true
  else begin
    let next_steps_lower_bound = match Array.max_elt remaining ~compare:Int.compare with
    | Some x -> x
    | None -> failwith "empty remaining array"
    in
    curr_s + next_steps_lower_bound >= best_s
  end

let accept remaining = 
  Array.for_all remaining ~f:(fun x -> x = 0)

let rec factorial x = 
  if x = 0 then 1 else x * factorial (x-1)

let comb n r = 
  let n_fact_over_r_fact = List.range (r+1) (n+1)
  |> List.fold ~init:1 ~f:(fun a b -> a * b)
  in
  let n_minus_r_fact = factorial (n-r) in
  n_fact_over_r_fact / n_minus_r_fact

let stars_and_bars n r = 
  comb (n + r - 1) n

let get_buttons_for_joltage buttons button_available joltage_idx = 
  List.range 0 (Array.length buttons)
  |> List.filter ~f:(fun i -> button_available.(i) && Array.exists buttons.(i) ~f:(fun j -> j = joltage_idx))
  |> List.to_array

let find_most_constrained_joltage buttons button_available remaining =
  let num_joltages = Array.length remaining in 
  let num_buttons_per_joltage = List.range 0 num_joltages
  |> List.map ~f:(fun i -> Array.length (get_buttons_for_joltage buttons button_available i))
  |> List.to_array
  in

  let ways = List.range 0 num_joltages
  |> List.map ~f:(fun i -> 
    let num_buttons = num_buttons_per_joltage.(i) in
    let rem = remaining.(i) in
    if num_buttons = 0 || rem = 0 then
      Int.max_value
    else stars_and_bars rem num_buttons
  )
  |> List.to_array
  in

  List.range 0 num_joltages
  |> List.fold ~init:0 ~f:(fun best_i curr_i ->
       if ways.(curr_i) < ways.(best_i) then curr_i else best_i
     )

let next_composition comp = 
  let i, _x = Array.findi_exn comp ~f:(fun _i x -> x > 0) in
  if i = (Array.length comp) - 1 then
    None
  else begin
    let next_comp = Array.copy comp in
    let v = next_comp.(i) in
    next_comp.(i+1) <- next_comp.(i+1) + 1;
    next_comp.(i) <- 0;
    next_comp.(0) <- v-1;
    Some next_comp
  end

let get_new_remaining remaining buttons buttons_indices comp = 
  let new_remaining = Array.copy remaining in
  comp
  |> Array.iteri ~f:(fun button_idx_idx num_presses ->
      let button_idx = buttons_indices.(button_idx_idx) in
      let button = buttons.(button_idx) in
      button
      |> Array.iter ~f:(fun i -> new_remaining.(i) <- new_remaining.(i) - num_presses)
    );
  new_remaining

let rec backtrack buttons remaining button_available best_s curr_s = 
  if reject remaining best_s curr_s then begin
    (* Printf.printf "reject, remaining: [";
    Array.iter remaining ~f:(fun x -> Printf.printf "%d," x);
    Printf.printf "], best_s: %d, curr_s: %d\n" best_s curr_s; *)
    best_s
  end else if accept remaining then 
    Int.min best_s curr_s
  else begin
    let best_joltage_idx = find_most_constrained_joltage buttons button_available remaining in
    let valid_button_indices = get_buttons_for_joltage buttons button_available best_joltage_idx in
    if (Array.length valid_button_indices) = 0 then
      best_s
    else begin
      let n = remaining.(best_joltage_idx) in
      (* let k = Array.length valid_button_indices in *)

      let new_curr_s = curr_s + n in

      let rec loop comp best_s_so_far = 
        let new_remaining = get_new_remaining remaining buttons valid_button_indices comp in
        let new_button_available = Array.copy button_available in
        Array.iter valid_button_indices ~f:(fun button_idx -> new_button_available.(button_idx) <- false);
        let curr_next_steps = backtrack buttons new_remaining new_button_available best_s_so_far new_curr_s in

        let new_best_s_so_far = Int.min best_s_so_far curr_next_steps in
        match next_composition comp with
        | Some next_comp -> loop next_comp new_best_s_so_far
        | None -> new_best_s_so_far
      in
      (* Printf.printf "remaining: [";
      Array.iter remaining ~f:(fun x -> Printf.printf "%d," x);
      Printf.printf "], best_s: %d, curr_s: %d\n" best_s curr_s; *)
      let start_comp = Array.create ~len:(Array.length valid_button_indices) 0 in
      start_comp.(0) <- n;
      let result = loop start_comp best_s in
      result
    end
  end
    

let solve_line line = 
  print_endline line;
  let joltages, buttons = parse_line line in
  let button_available = Array.create ~len:(Array.length buttons) true in
  backtrack buttons joltages button_available 999999 0


let main () : string = 
  In_channel.read_lines file
  |> List.fold ~init:0 ~f:(fun total line ->
    total + (solve_line line)
    )
  |> Int.to_string

(* 
Idea: 
1) At each step of the recursion, choose the joltage that has the least number of distributions (using stars and bars)
2) Iterate over all these distributions 
3) Recurse
*) 
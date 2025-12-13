open Core

(* This is my OCaml implementation of this amazing solution here
https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/

side note: glad I don't need my python linear programming solution anymore, now everything is ocaml!
 *)

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
    |> List.rev
    |> List.map ~f:parse_button_str
    |> List.to_array
    in
    joltages, buttons
  | _ -> failwith "invalid line to parse"


let button_indices_to_joltage_change buttons num_joltages button_indices = 
  let diff = Array.create ~len:num_joltages 0 in
  Array.iter button_indices ~f:(fun button_idx -> 
      let button = buttons.(button_idx) in
      Array.iter button ~f:(fun joltage_idx -> diff.(joltage_idx) <- diff.(joltage_idx) + 1)
    );
  diff

let create_mask_to_diffs_map joltages buttons = 
  let num_joltages = Array.length joltages in
  let mask_to_button_groups = Array.create ~len:(1 lsl num_joltages) [] in

  let rec find_0_1_button_combinations mask buttons_pressed button_idx = 
    if button_idx = (Array.length buttons) then begin
      mask_to_button_groups.(mask) <- buttons_pressed :: mask_to_button_groups.(mask)
    end else
      begin
      (* case 1, don't press this button *)
      find_0_1_button_combinations mask buttons_pressed (button_idx+1);

      (* case 2, press this button *)
      let button = buttons.(button_idx) in
      let new_mask = button
      |> Array.fold ~init:mask ~f:(fun new_mask joltage_idx -> new_mask lxor (1 lsl joltage_idx))
      in
      find_0_1_button_combinations new_mask (button_idx :: buttons_pressed) (button_idx+1)
    end
  in

  find_0_1_button_combinations 0 [] 0;

  mask_to_button_groups
  |> Array.map ~f:(fun button_groups -> 
    button_groups
    |> List.to_array 
    |> Array.map ~f:(fun button_indices ->
      let diff = button_indices 
      |> List.to_array 
      |> (button_indices_to_joltage_change buttons (Array.length joltages)) 
      in
      let num_buttons_pressed = List.length button_indices in
      num_buttons_pressed, diff
    ))

let get_odd_mask_from_joltages joltages = 
  Array.foldi joltages ~init:0 ~f:(fun i mask x ->
    if x land 1 = 1 then
      mask lor (1 lsl i)
    else mask
    )

let rec dfs memo mask_to_diffs joltages = 
  if Array.exists joltages ~f:(fun x -> x < 0) then
    99999
  else if Array.for_all joltages ~f:(Int.equal 0) then
    0
  else begin
    match Hashtbl.Poly.find memo joltages with
    | Some x -> x
    | None ->
      let mask = get_odd_mask_from_joltages joltages in
      let res = mask_to_diffs.(mask)
      |> Array.fold ~init:999999 ~f:(fun acc (num_pressed, diff) ->
        let new_joltages = Array.map2_exn joltages diff ~f:(fun existing d -> (existing - d) / 2) in
        Int.min acc (num_pressed + 2 * (dfs memo mask_to_diffs new_joltages))
        )
      in
      Hashtbl.Poly.set memo ~key:joltages ~data:res;
      res
  end


let solve_line line = 
  let joltages, buttons = parse_line line in
  let mask_to_diffs = create_mask_to_diffs_map joltages buttons in
  
  let memo = Hashtbl.Poly.create () in
  dfs memo mask_to_diffs joltages

let main () : string = 
  In_channel.read_lines file
  |> List.map ~f:solve_line
  |> List.fold ~init:0 ~f:Int.(+)
  |> Int.to_string
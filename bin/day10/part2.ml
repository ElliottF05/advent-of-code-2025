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
    |> List.map ~f:parse_button_str
    |> List.to_array
    in
    joltages, buttons
  | _ -> failwith "invalid line to parse"

let button_to_mask button = 
  button
  |> Array.fold ~init:0 ~f:(fun acc i -> acc lor (1 lsl i))

let fill_mask_to_diffs_map mask_to_diffs num_joltages buttons = 
  Array.iter mask_to_diffs ~f:(fun q -> Queue.clear q);

  let button_masks = buttons
  |> Array.map ~f:button_to_mask
  in

  let rec backtrack button_idx num_pressed diff mask = 
    if button_idx = (Array.length buttons) then begin
      Queue.enqueue mask_to_diffs.(mask) (num_pressed, diff)
    end else begin
      (* case 1, don't choose this button *)
      backtrack (button_idx + 1) num_pressed diff mask;

      (* case 2, choose this button *)
      let new_mask = mask lxor button_masks.(button_idx) in
      let new_diff = Array.copy diff in
      Array.iter buttons.(button_idx) ~f:(fun i -> new_diff.(i) <- new_diff.(i) + 1);
      backtrack (button_idx + 1) (num_pressed + 1) new_diff new_mask;
    end
  in

  backtrack 0 0 (Array.create ~len:num_joltages 0) 0

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
      |> Queue.fold ~init:999999 ~f:(fun acc (num_pressed, diff) ->
        let new_joltages = Array.map2_exn joltages diff ~f:(fun existing d -> (existing - d) / 2) in
        Int.min acc (num_pressed + 2 * (dfs memo mask_to_diffs new_joltages))
        )
      in
      Hashtbl.Poly.set memo ~key:joltages ~data:res;
      res
  end


let solve_line mask_to_diffs line = 
  let joltages, buttons = parse_line line in
  fill_mask_to_diffs_map mask_to_diffs (Array.length joltages) buttons;
  let memo = Hashtbl.Poly.create () in
  dfs memo mask_to_diffs joltages

let main () : string = 
  let mask_to_diffs = Array.init 1024 ~f:(fun _ -> Queue.create ~capacity:100 ()) in
  In_channel.read_lines file
  |> List.map ~f:(solve_line mask_to_diffs)
  |> List.fold ~init:0 ~f:Int.(+)
  |> Int.to_string
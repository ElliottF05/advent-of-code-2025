open Core

let file = "./bin/day10/part1.txt"

let parse_light_str light_str = 
  let bits, _ = light_str
  |> String.to_list
  |> List.filter ~f:(fun c -> Char.(<>) c '[' && Char.(<>) c ']')
  |> List.fold ~init:(0, 1) ~f:(fun (bits, mask) c ->
    match c with 
    | '#' -> (bits lor mask, mask lsl 1)
    | _ -> (bits, mask lsl 1)
    )
  in
  bits

let parse_button_str button_str = 
  button_str
  |> String.to_list
  |> List.filter ~f:Char.is_digit
  |> List.map ~f:(fun c -> (Char.to_int c) - (Char.to_int '0'))
  |> List.fold ~init:0 ~f:(fun bits x -> bits lor (1 lsl x))

let parse_line line = 
  let lights, remaining = match String.split line ~on:' ' with
  | hd :: tl -> hd, tl
  | _ -> failwith "invalid line to parse"
  in
  let buttons = List.take_while ~f:(fun s -> String.contains s '(') remaining in

  let light_bits = parse_light_str lights in
  let button_bits = List.map buttons ~f:parse_button_str in
  light_bits, button_bits

let rec backtrack light_bits acc button_bits = 
  match button_bits with 
  | [] -> if acc = light_bits then 0 else 999999
  | curr_button :: next_button_bits -> 
    Int.min 
      (1 + backtrack light_bits (acc lxor curr_button) next_button_bits)
      (backtrack light_bits acc next_button_bits)

let solve_line line = 
  let light_bits, button_bits = parse_line line in
  backtrack light_bits 0 button_bits


let main () : string = 
  In_channel.read_lines file
  |> List.fold ~init:0 ~f:(fun total line ->
    total + (solve_line line)
    )
  |> Int.to_string
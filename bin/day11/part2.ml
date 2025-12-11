open Core

let file = "./bin/day11/part1.txt"

let parse_line line = 
  let colon_idx = match String.index line ':' with
  | Some colon_idx -> colon_idx
  | None -> failwith "invalid line, expected colon"
  in

  let name = String.slice line 0 colon_idx in
  let out_names = String.slice line (colon_idx + 2) 0 
  |> String.split ~on:' '
  in

  name, out_names

let rec dfs adj_map dp curr seen_dac seen_fft = 
  if String.(=) curr "out" then 
    if seen_dac && seen_fft then 1 else 0
  else
    match Hashtbl.find dp (curr, seen_dac, seen_fft) with
    | Some x -> x
    | None -> 
      match Hashtbl.find adj_map curr with
      | None -> 0
      | Some next_names -> 
        let new_seen_dac = seen_dac || (String.equal curr "dac") in
        let new_seen_fft = seen_fft || (String.equal curr "fft") in
        let res = List.fold next_names ~init:0 ~f:(fun acc nxt -> acc + (dfs adj_map dp nxt new_seen_dac new_seen_fft)) in
        Hashtbl.set dp ~key:(curr, seen_dac, seen_fft) ~data:res;
        res

let main () : string = 
  let lines = In_channel.read_lines file in

  let adj_list = List.map lines ~f:parse_line in
  let adj_map = Hashtbl.of_alist_exn (module String) adj_list in
  let dp = Hashtbl.Poly.create () in
  let result = dfs adj_map dp "svr" false false in
  Int.to_string result
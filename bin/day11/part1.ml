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

let rec dfs adj_map dp curr = 
  if String.(=) curr "out" then 
    1
  else
    match Hashtbl.find dp curr with
    | Some x -> x
    | None -> 
      match Hashtbl.find adj_map curr with
      | None -> 0
      | Some next_names -> 
        let res = List.fold next_names ~init:0 ~f:(fun acc nxt -> acc + (dfs adj_map dp nxt)) in
        Hashtbl.set dp ~key:curr ~data:res;
        res

let main () : string = 
  let lines = In_channel.read_lines file in

  let adj_list = List.map lines ~f:parse_line in
  let adj_map = Hashtbl.of_alist_exn (module String) adj_list in
  let dp = Hashtbl.create (module String) in

  let result = dfs adj_map dp "you" in
  Int.to_string result
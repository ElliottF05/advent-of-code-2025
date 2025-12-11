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

let rec dfs adj_map dp dest curr = 
  if String.(=) curr dest then 
    1
  else
    match Hashtbl.find dp curr with
    | Some x -> x
    | None -> 
      match Hashtbl.find adj_map curr with
      | None -> 0
      | Some next_names -> 
        let res = List.fold next_names ~init:0 ~f:(fun acc nxt -> acc + (dfs adj_map dp dest nxt)) in
        Hashtbl.set dp ~key:curr ~data:res;
        res

let get_ways adj_map src dest = 
  let dp = Hashtbl.create (module String) in
  dfs adj_map dp dest src

let get_multi_ways adj_map path = 
  let src, next_names = match path with
  | hd :: tl -> hd, tl
  | [] -> failwith "path has length < 2"
  in

  let _, num_ways = List.fold next_names ~init:(src, 1) ~f:(fun (prev_name, ways) next_name -> 
    let new_ways = get_ways adj_map prev_name next_name in
    next_name, ways * new_ways
    )
  in
  num_ways

let main () : string = 
  let lines = In_channel.read_lines file in

  let adj_list = List.map lines ~f:parse_line in
  let adj_map = Hashtbl.of_alist_exn (module String) adj_list in

  let paths = [
    ["svr"; "dac"; "fft"; "out"];
    ["svr"; "fft"; "dac"; "out"]
  ]
  in

  let ways = List.map paths ~f:(get_multi_ways adj_map) in
  let result = List.fold ways ~init:0 ~f:Int.(+) in

  Int.to_string result
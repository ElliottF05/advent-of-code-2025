open Core

let file = "./bin/day8/part1.txt"

let parse_line line = 
  match String.split line ~on:',' with
  | [a; b; c] -> (Int.of_string a, Int.of_string b, Int.of_string c)
  | _ -> failwith "invalid line"

let get_dist_squared positions i j = 
  let x1, y1, z1 = positions.(i) in
  let x2, y2, z2 = positions.(j) in
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  let dz = z2 - z1 in
  dx*dx + dy*dy + dz*dz

let find_next_idx status = 
  let next_idx, _ = Array.foldi 
    status 
    ~init:(-1, Int.max_value) 
    ~f:(fun i (prev_i, prev_dist) (is_seen, _, new_dist) -> 
      if (not is_seen) && new_dist < prev_dist then
        (i, new_dist)
      else
        (prev_i, prev_dist)
  ) in
  next_idx

let update_dists positions status next_idx = 
  Array.iteri status ~f:(fun i (is_seen, _, existing_dist) -> 
    if not is_seen then
      let new_dist = get_dist_squared positions i next_idx in
      if new_dist < existing_dist then
        status.(i) <- (false, next_idx, new_dist)
  )

let main () : string = 
  let positions = In_channel.read_lines file 
  |> List.map ~f:parse_line
  |> List.to_array
  in

  (* status holds tuple (seen, prev_index, dist) *)
  let status = Array.init (Array.length positions) ~f:(fun i -> (false, 0, get_dist_squared positions 0 i)) in
  status.(0) <- (true, 0, 0);

  let i, j, _ =  
  List.range 1 (Array.length positions)
  |> List.fold
    ~init:(0, 0, 0)  
    ~f:(fun (prev_i, prev_j, prev_dist) _ ->

      let next_idx = find_next_idx status in
      let _, prev_idx, new_dist = status.(next_idx) in
      status.(next_idx) <- (true, 0, 0);
      update_dists positions status next_idx;

      if new_dist > prev_dist then
        (prev_idx, next_idx, new_dist)
      else
        (prev_i, prev_j, prev_dist)
    );
  in

  let x1, _, _ = positions.(i) in
  let x2, _, _ = positions.(j) in

  Int.to_string (x1 * x2)

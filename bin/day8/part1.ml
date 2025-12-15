open Core

let file = "./bin/day8/part1.txt"
let num_edges = 1000

let parse_line line = 
  let vals = line
  |> String.split ~on:','
  |> List.map ~f:Int.of_string
  in

  match vals with
  | [a; b; c] -> (a,b,c)
  | _ -> failwith "invalid line"

let get_dist_squared positions i j = 
  let x1, y1, z1 = positions.(i) in
  let x2, y2, z2 = positions.(j) in
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  let dz = z2 - z1 in
  dx*dx + dy*dy + dz*dz

let build_edges positions = 
  let heap = Pairing_heap.create ~min_size:1024 ~cmp:(fun (_, _, d1) (_, _, d2) -> Int.compare d2 d1) () in
  let n = Array.length positions in
  for i = 0 to (n-1) do
    for j = 0 to (i-1) do
      let new_dist = get_dist_squared positions i j in
      if Pairing_heap.length heap < num_edges then
        Pairing_heap.add heap (i, j, new_dist)
      else begin
        let (_, _, prev_dist) = Pairing_heap.top_exn heap in
        if new_dist < prev_dist then begin
          Pairing_heap.remove_top heap;
          Pairing_heap.add heap (i, j, new_dist)
        end
      end 
    done;
  done;

  Pairing_heap.to_array heap


let rec find parents i = 
  let root = parents.(i) in
  if root <> i then
    begin
      parents.(i) <- find parents root;
      parents.(i)
    end
  else 
    root

let union parents i j = 
  let i_root = find parents i in
  let j_root = find parents j in

  if i_root <> j_root then 
    parents.(i_root) <- j_root

let main () = 
  let lines = In_channel.read_lines file in

  let positions = lines |> List.map ~f:parse_line |> Array.of_list in

  let edges = build_edges positions in
  let parents = Array.init (Array.length positions) ~f:(fun i -> i) in
  Array.iter edges ~f:(fun (i, j, _) -> union parents i j);

  Array.map_inplace parents ~f:(fun p -> find parents p);

  let counts = Array.create ~len:(Array.length parents) 0 in
  Array.iter parents ~f:(fun p -> counts.(p) <- counts.(p) + 1);
  Array.sort counts ~compare:(fun a b -> Int.compare b a);

  let result = counts.(0) * counts.(1) * counts.(2) in

  string_of_int result
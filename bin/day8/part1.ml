open Core

let file = "./bin/day8/part1.txt"

module Edge = struct
  type t = {
    i: int;
    j: int;
    len_squared: int
  }

  let make positions i j = 
    let len_squared = 
      let (x1, y1, z1) = positions.(i) in
      let (x2, y2, z2) = positions.(j) in
      let dx = x2 - x1 in
      let dy = y2 - y1 in
      let dz = z2 - z1 in
      dx*dx + dy*dy + dz*dz
    in
    { i; j; len_squared }
end

let line_to_pos line = 
  let vals = line
  |> String.split_on_chars ~on:[',']
  |> List.map ~f:int_of_string
  in

  match vals with
  | a :: b :: c :: [] -> (a,b,c)
  | _ -> failwith "invalid line"

let build_edges positions = 
  let range = List.range 0 (Array.length positions) |> List.to_array in
  let pairs = Array.cartesian_product range range in

  pairs
  |> Array.filter ~f:(fun (i,j) -> i < j)
  |> Array.map ~f:(fun (i,j) -> Edge.make positions i j)


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

  let positions = lines |> List.map ~f:line_to_pos |> Array.of_list in

  let edges = build_edges positions in

  let heap = Pairing_heap.of_array edges ~cmp:(fun e1 e2 -> Int.compare e1.len_squared e2.len_squared) in
  let parents = Array.init (Array.length positions) ~f:(fun i -> i) in

  List.range 0 1000
  |> List.iter ~f:(fun _ ->
      match Pairing_heap.top heap with
      | None -> ()
      | Some edge -> begin
          union parents edge.i edge.j;
          Pairing_heap.remove_top heap;
        end
    )
  ;

  Array.map_inplace parents ~f:(fun p -> find parents p);

  let counts = Array.create ~len:(Array.length parents) 0 in
  Array.iter parents ~f:(fun p -> counts.(p) <- counts.(p) + 1);
  Array.sort counts ~compare:(fun a b -> Int.compare b a);

  let result = counts.(0) * counts.(1) * counts.(2) in

  string_of_int result
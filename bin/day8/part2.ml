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
      let dx =x2 - x1 in
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
  let range = List.range 0 (Array.length positions) in
  let pairs = List.cartesian_product range range in

  pairs
  |> List.map ~f:(fun (p1, p2) -> Edge.make positions p1 p2)
  |> List.filter ~f:(fun e -> e.Edge.i < e.Edge.j)


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
    begin
      parents.(i_root) <- j_root;
      true
    end
  else
    false

let rec add_edges positions heap parents rem = 
  (* print_endline (string_of_int rem); *)
  let top = Pairing_heap.top heap in
  Pairing_heap.remove_top heap;
  match top with
  | None -> -1
  | Some edge -> 
    if union parents edge.Edge.i edge.Edge.j then
      if rem - 1 = 0 then
        begin
          let x1, _, _ = positions.(edge.Edge.i) in
          let x2, _, _ = positions.(edge.Edge.j) in
          x1 * x2
        end
      else
        add_edges positions heap parents (rem-1)
    else
      add_edges positions heap parents rem


let main () = 
  let lines = In_channel.read_lines file in

  let positions = lines |> List.map ~f:line_to_pos |> Array.of_list in

  let edges = build_edges positions in

  let heap = Pairing_heap.of_list edges ~cmp:(fun e1 e2 -> Int.compare e1.len_squared e2.len_squared) in
  let parents = Array.init (Array.length positions) ~f:(fun i -> i) in

  let result = add_edges positions heap parents ((Array.length positions) - 1) in

  string_of_int result
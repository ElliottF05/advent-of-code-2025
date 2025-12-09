open Core

let file = "./bin/day9/part1.txt"

let line_to_pos line = 
  match String.split ~on:',' line with
  | [x ; y] -> (Int.of_string x, Int.of_string y)
  | _ -> failwith "invalid line"

let get_rect_size (pos1, pos2) = 
  let x1, y1 = pos1 in
  let x2, y2 = pos2 in
  let dx = Int.abs(x2 - x1) + 1 in
  let dy = Int.abs(y2 - y1) + 1 in
  dx * dy


let get_rectangle_sizes positions = 
  List.cartesian_product positions positions
  |> List.map ~f:get_rect_size

let main () = 
  let lines = In_channel.read_lines file in

  let positions = List.map ~f:line_to_pos lines in

  let sizes = get_rectangle_sizes positions in

  let max_size = List.max_elt ~compare:Int.compare sizes in

  match max_size with
  | Some x -> Int.to_string x
  | None -> failwith "no rectangle sizes found"
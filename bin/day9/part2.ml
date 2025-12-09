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

let get_top_left_idx positions = 
  let seed_index, _seed_x, _seed_y = positions
  |> Array.foldi ~init:(-1, Int.max_value, Int.max_value) ~f:(fun i (j, px, py) (x, y) -> 
    if y < py || (y = py && x < px) then
      (i, x, y)
    else
      (j, px, py)
    )
  in
  seed_index

let get_out_dir positions i =
  let n = Array.length positions in
  let j = Int.(%) (i+1) n in

  let x,y = positions.(i) in
  let nx,ny = positions.(j) in

  let dx = nx - x in
  let dy = ny - y in

  if dx = 0 then
    if dy > 0 then (0, 1) else (0, -1)
  else
    if dx > 0 then (1,0) else (-1, 0)

let get_in_dir positions i = 
  let n = Array.length positions in
  let j = Int.(%) (i-1) n in
  get_out_dir positions j

let cross dir1 dir2 = 
  let dx1, dy1 = dir1 in
  let dx2, dy2 = dir2 in
  dx1*dy2 - dy1*dx2


let get_outer_turn_sign positions top_left_idx = 
  let in_dir = get_in_dir positions top_left_idx in
  let out_dir = get_out_dir positions top_left_idx in
  let c = cross in_dir out_dir in
  if c < 0 then -1 else 1

let get_corners positions outer_turn_sign = 
  let n = Array.length positions in
  List.range 0 n
  |> List.map ~f:(fun i ->
      let dx_in, dy_in = get_in_dir positions i in
      let dx_out, dy_out = get_out_dir positions i in
      let x,y = positions.(i) in

      let cross_val = cross (dx_in, dy_in) (dx_out, dy_out) in
      let is_outer_turn = outer_turn_sign * cross_val > 0 in

      if is_outer_turn then
        (x + dx_in - dx_out, y + dy_in - dy_out)
      else
        (x - dx_in + dx_out, y - dy_in + dy_out)
    )
  |> List.to_array

let between a b x = 
  let lo = Int.min a b in
  let hi = Int.max a b in
  lo <= x && x <= hi

let ranges_overlap a1 a2 b1 b2 = 
  let lo1 = Int.min a1 a2 in
  let hi1 = Int.max a1 a2 in
  let lo2 = Int.min b1 b2 in
  let hi2 = Int.max b1 b2 in
  lo1 <= hi2 && lo2 <= hi1

let is_horizontal (x1, y1) (x2, y2) =
  y1 = y2 && x1 <> x2

let lines_intersect (ax1, ay1) (ax2, ay2) (bx1, by1) (bx2, by2) = 
  let a_h = is_horizontal (ax1, ay1) (ax2, ay2) in
  let b_h = is_horizontal (bx1, by1) (bx2, by2) in
  match a_h, b_h with 
  | true, true -> 
    (* both horizontal *)
    ay1 = by1 && (ranges_overlap ax1 ax2 bx1 bx2)
  | false, false ->
    (* both vertical *)
    ax1 = bx1 && (ranges_overlap ay1 ay2 by1 by2)
  | true, false ->
    (* a is horizontal, b is vertical *)
    (between ax1 ax2 bx1) && (between by1 by2 ay1)
  | false, true ->
    (* a is vertical, b is horizontal *)
    (between ay1 ay2 by1) && (between bx1 bx2 ax1)

let rect_edges (x1, y1) (x2, y2) = 
  [
    ((x1, y1), (x1, y2)) ;
    ((x1, y1), (x2, y1)) ;
    ((x2, y2), (x1, y2)) ;
    ((x2, y2), (x2, y1)) ;
  ]
 
let rectangle_intersects_line (corner1, corner2) (line_start, line_end) = 
  let edges = rect_edges corner1 corner2 in
  edges
  |> List.exists ~f:(fun (e1, e2) -> lines_intersect e1 e2 line_start line_end)


let rectangle_is_valid corner_pairs rectangle =
  not (Array.exists ~f:(rectangle_intersects_line rectangle) corner_pairs)

let main () = 
  let lines = In_channel.read_lines file in

  let positions = lines
  |> List.map ~f:line_to_pos
  |> List.to_array
  in

  let top_left_idx = get_top_left_idx positions in
  let outer_turn_sign = get_outer_turn_sign positions top_left_idx in
  let corners = get_corners positions outer_turn_sign in

  let corner_pairs = List.range 0 (Array.length corners)
  |> List.map ~f:(fun i ->
    let j = Int.(%) (i+1) (Array.length corners) in
    (corners.(i), corners.(j))
    )
  |> List.to_array
  in

  let range = List.range 0 (Array.length positions) in
  let rectangles = List.cartesian_product range range
  |> List.filter ~f:(fun (i,j) -> i < j)
  |> List.map ~f:(fun (i,j) -> (positions.(i), positions.(j)))
  |> List.to_array
  in
  
  let valid_rectangles = rectangles
  |> Array.filter ~f:(rectangle_is_valid corner_pairs)
  in

  let best_rect_opt = valid_rectangles
  |> Array.max_elt ~compare:(fun r1 r2 -> Int.compare (get_rect_size r1) (get_rect_size r2))
  in

  let best_rect = match best_rect_opt with
  | Some r -> r
  | None -> failwith "no valid rectangles found"
  in


  Int.to_string (get_rect_size best_rect)

(* 4_629_483_216 is too high *)
(* 1_525_991_432 is correct *)
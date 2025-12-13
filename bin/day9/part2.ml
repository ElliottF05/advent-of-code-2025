open Core

let file = "./bin/day9/part1.txt"

let line_to_pos line = 
  match String.split ~on:',' line with
  | [x ; y] -> (Int.of_string x, Int.of_string y)
  | _ -> failwith "invalid line"

let get_idx_to_pos_maps positions = 
  let x_positions = positions
  |> Array.map ~f:(fun (x,_y) -> x)
  |> Array.to_list
  |> List.dedup_and_sort ~compare:Int.compare
  |> List.to_array
  in

  let y_positions = positions
  |> Array.map ~f:(fun (_x,y) -> y)
  |> Array.to_list
  |> List.dedup_and_sort ~compare:Int.compare
  |> List.to_array
  in

  x_positions, y_positions

let get_pos_to_idx_map arr = 
  let map = List.range 0 (Array.length arr)
  |> Hashtbl.create_mapped (module Int) ~growth_allowed:false ~get_key:(fun i -> arr.(i)) ~get_data:(fun i -> i)
  in

  match map with
  | `Ok x -> x
  | _ -> failwith "duplicate keys"

let get_idx x_to_col y_to_row (x, y) = 
  Hashtbl.find_exn x_to_col x, Hashtbl.find_exn y_to_row y

let fill_edge grid (c1, r1) (c2, r2) = 
  if c1 = c2 then
    List.range ~stop:`inclusive (Int.min r1 r2) (Int.max r1 r2)
    |> List.iter ~f:(fun r -> grid.(c1).(r) <- true)
  else
    List.range ~stop:`inclusive (Int.min c1 c2) (Int.max c1 c2)
    |> List.iter ~f:(fun c -> grid.(c).(r1) <- true)

let fill_grid_edges grid positions x_to_col y_to_row = 
  List.range 0 (Array.length positions)
  |> List.iter ~f:(fun i -> 
    let j = Int.(%) (i+1) (Array.length positions) in
    let idx1 = get_idx x_to_col y_to_row positions.(i) in
    let idx2 = get_idx x_to_col y_to_row positions.(j) in
    fill_edge grid idx1 idx2;
    )

let find_seed_cell grid = 
  let rec scan c r = 
    if r >= (Array.length grid.(0)) then
      scan (c+1) 0
    else
      if grid.(c).(r) then
        c+1,r+1
      else
        scan (c+1) r
  in
  scan 0 0

let in_bounds grid col row = 
  0 <= col && col < (Array.length grid) && 0 <= row && row < Array.length grid.(0)
  
let rec flood_fill grid col row = 
  if (in_bounds grid col row) && (not grid.(col).(row)) then
    begin
      grid.(col).(row) <- true;
      flood_fill grid (col+1) row;
      flood_fill grid (col-1) row;
      flood_fill grid col (row+1);
      flood_fill grid col (row-1);
    end

let build_prefix prefix grid = 
  List.range 0 (Array.length grid)
  |> List.iter ~f:(fun c ->
    List.range 0 (Array.length grid.(0))
    |> List.iter ~f:(fun r ->
        let curr_val = if grid.(c).(r) then 1 else 0 in
        prefix.(c+1).(r+1) <- prefix.(c+1).(r) + prefix.(c).(r+1) - prefix.(c).(r) + curr_val;
      )
    )

let normalized_corners (x1, y1) (x2, y2) = 
  let left = Int.min x1 x2 in
  let right = Int.max x1 x2 in
  let top = Int.min y1 y2 in
  let bottom = Int.max y1 y2 in
  (left, top), (right, bottom)

let get_rect_area (x1, y1) (x2, y2) = 
  (x2 - x1 + 1) * (y2 - y1 + 1)

let get_rect_sum prefix (c1, r1) (c2, r2) = 
  prefix.(c2+1).(r2+1) - prefix.(c1).(r2+1) - prefix.(c2+1).(r1) + prefix.(c1).(r1)

let main () : string = 
  let positions = In_channel.read_lines file
  |> List.map ~f:line_to_pos
  |> List.to_array
  in

  let col_to_x, row_to_y = get_idx_to_pos_maps positions in
  let x_to_col = get_pos_to_idx_map col_to_x in
  let y_to_row = get_pos_to_idx_map row_to_y in

  let rows = Array.length row_to_y in
  let cols = Array.length col_to_x in

  let grid = Array.make_matrix ~dimx:cols ~dimy:rows false in
  fill_grid_edges grid positions x_to_col y_to_row;

  (* List.range 0 (Array.length grid) 
  |> List.iter ~f:(fun r ->
    List.range 0 (Array.length grid)
    |> List.iter ~f:(fun c ->
      Printf.printf "%s" (if grid.(c).(r) then "X" else ".");
      if c = (Array.length grid) - 1 then
        Printf.printf "\n"
      
      )
    ); *)

  let seed_c, seed_r = find_seed_cell grid in
  flood_fill grid seed_c seed_r;

  let prefix = Array.make_matrix ~dimx:(cols+1) ~dimy:(rows+1) 0 in
  build_prefix prefix grid;

  let result = ref 0 in
  Array.cartesian_product positions positions 
  |> Array.iter ~f:(fun (unnormalized_pos1, unnormalized_pos2) ->
    let pos1, pos2 = normalized_corners unnormalized_pos1 unnormalized_pos2 in
    let full_area = get_rect_area pos1 pos2 in
    if full_area > !result then
      begin
        let idx1 = get_idx x_to_col y_to_row pos1 in
        let idx2 = get_idx x_to_col y_to_row pos2 in
        let reduced_area = get_rect_area idx1 idx2 in
        let covered_area = get_rect_sum prefix idx1 idx2 in
        if covered_area = reduced_area then
          result := full_area;
      end
    )
  ;

  Int.to_string !result

(* 4_629_483_216 is too high *)
(* 1_525_991_432 is correct *)
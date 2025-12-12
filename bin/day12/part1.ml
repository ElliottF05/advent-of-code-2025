open Core

let file = "./bin/day12/part1.txt"

let parse_region_line line = 
  let semicolon_pos = String.index_exn line ':' in
  let dims = String.slice line 0 semicolon_pos
  |> String.split ~on:'x'
  |> List.map ~f:Int.of_string
  in

  let area = match dims with
  | x :: y :: [] -> x * y
  | _ -> failwith "invalid dims"
  in

  let shape_counts = String.slice line (semicolon_pos + 2) 0 
  |> String.split ~on:' '
  |> List.map ~f:Int.of_string
  in

  area , shape_counts

let parse_shape_str (lines : string list) = 
  lines
  |> List.fold ~init:0 ~f:(fun acc line -> 
    acc + (String.count line ~f:(Char.equal '#'))
    )

let filter_region shape_areas (rectangle_area, shape_counts) = 
  let min_covered_area = List.fold2_exn shape_counts shape_areas ~init:0 ~f:(fun acc c a -> acc + c * a) in
  min_covered_area <= rectangle_area


let main () : string = 
  let lines = In_channel.read_lines file in
  let shapes_strs, regions_strs = lines
  |> List.fold ~init:([], []) ~f:(fun (groups, curr_group) line -> 
    match line with
    | "" -> ((List.rev curr_group) :: groups, [])
    | _ -> (groups, line :: curr_group)
    )
  in

  let shapes_strs = List.rev shapes_strs in
  let regions_strs = List.rev regions_strs in

  let regions = regions_strs
  |> List.map ~f:parse_region_line
  in

  let shape_areas = shapes_strs
  |> List.map ~f:parse_shape_str
  in

  let result = regions
  |> List.filter ~f:(filter_region shape_areas)
  |> List.length
  in

  Int.to_string result


(* 16 is not correct *)
(* 403 is correct though, shocked this worked
  Note to self: always check for un-specficied constraints in the problem
*)
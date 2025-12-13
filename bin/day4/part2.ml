open Core

let file = "./bin/day4/part1.txt"

let in_bounds grid (r, c) = 
  let rows = Array.length grid in
  let cols = Array.length (Array.get grid 0) in
  0 <= r && r < rows && 0 <= c && c < cols

let count_neighbors (grid : char array array) (r, c) : int =
  let dirs = [|
    (-1,-1); (-1,0); (-1,1);
    (0,-1) ;         (0,1) ;
    (1, -1); (1,0) ; (1,1) ;
  |]
  in

  dirs
  |> Array.map ~f:(fun (dr,dc) -> if in_bounds grid (r+dr,c+dc) && Char.equal grid.(r+dr).(c+dc) '@' then 1 else 0)
  |> Array.fold ~init:0 ~f:Int.(+)

let rec dfs grid r c = 
  if not (in_bounds grid (r, c) && Char.equal grid.(r).(c) '@') then
    0
  else begin
    if count_neighbors grid (r,c) < 4 then begin
      let dirs = [|
        (-1,-1); (-1,0); (-1,1);
        (0,-1) ;         (0,1) ;
        (1, -1); (1,0) ; (1,1) ;
      |]
      in
      grid.(r).(c) <- '.';
      Array.fold dirs ~init:1 ~f:(fun acc (dr,dc) -> acc + dfs grid (r+dr) (c+dc))
    end else
      0
  end

let main () : string = 
  let lines = In_channel.read_lines file in

  let grid = lines
  |> List.to_array
  |> Array.map ~f:String.to_array
  in

  let rows = List.range 0 (Array.length grid) |> List.to_array in
  let cols = List.range 0 (Array.length grid.(0)) |> List.to_array in

  Array.cartesian_product rows cols
  |> Array.map ~f:(fun (r,c) -> dfs grid r c)
  |> Array.fold ~init:0 ~f:Int.(+)
  |> Int.to_string
let file = "./bin/day4/part1.txt"

let in_bounds grid (r, c) = 
  let rows = Array.length grid in
  let cols = Array.length (Array.get grid 0) in
  0 <= r && r < rows && 0 <= c && c < cols

let count_neighbors (grid : char array array) (r, c) : int =
  let dirs = [
    (-1,-1); (-1,0); (-1,1);
    (0,-1) ;         (0,1) ;
    (1, -1); (1,0) ; (1,1) ;
  ]
  in

  dirs
  |> List.map (fun (dr, dc) -> (r + dr, c + dc))
  |> List.filter (in_bounds grid) (* currying! *)
  |> List.filter (fun (r,c) -> grid.(r).(c) = '@')
  |> List.length

let main () : string = 
  let file_content = In_channel.with_open_text file In_channel.input_all in
  let lines = String.split_on_char '\n' file_content in

  let grid = lines
  |> List.map (fun line -> line |> String.to_seq |> Array.of_seq)
  |> Array.of_list
  in

  let rows, cols = Utils.get_rows_and_cols grid in

  let result = Utils.range_over_2d rows cols
  |> Seq.filter (fun (r,c) -> grid.(r).(c) = '@')
  |> Seq.map (count_neighbors grid)
  |> Seq.filter (fun x -> x < 4)
  |> Seq.length
  in

  string_of_int result
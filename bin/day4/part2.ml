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

  let rec loop () = 
    let positions_to_remove = Utils.range_over_2d rows cols
    |> Seq.filter (fun (r,c) -> grid.(r).(c) = '@')
    |> Seq.filter (fun (r,c) -> count_neighbors grid (r,c) < 4)
    |> List.of_seq
    in

    let removed = List.length positions_to_remove in

    positions_to_remove
    |> List.iter (fun (r,c) -> grid.(r).(c) <- '.');

    if removed = 0 then
      0
    else
      removed + loop ()
  in

  let result = loop () in

  string_of_int result
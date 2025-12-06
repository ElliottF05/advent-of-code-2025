let file = "./bin/day6/part1.txt"

let main () : string = 
  let ic = open_in file in

  let rec loop () = 
    try
      let line = input_line ic in
      match line with
      | "" -> []
      | _ -> line :: loop ()
    with End_of_file ->
      []
  in

  let lines = loop () in

  let rows = List.take (List.length lines - 1) lines
  |> List.map (fun row -> row
      |> String.split_on_char ' ' 
      |> List.filter (fun s -> String.length s > 0)
      |> List.map String.trim
      |> List.map int_of_string
    )
  in

  let ops = List.nth lines (List.length lines - 1)
  |> String.split_on_char ' '
  |> List.filter (fun s -> String.length s > 0)
  |> List.map String.trim
  in

  let initial_state = ops
  |> List.map (fun o -> if o = "*" then 1 else 0)
  |> List.map2 (fun o p -> (o, p)) ops
  in

  let result = rows
  |> List.fold_left (fun state row -> 
    let new_state = List.map2 (fun (o,p) x -> if o = "*" then (o, p * x) else (o, p + x)) state row in
    new_state
    ) initial_state
  |> List.map (fun (_o,p) -> p)
  |> List.fold_left ( + ) 0
  in

  string_of_int result
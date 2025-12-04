let file = "./bin/day1/part1.txt"

let main(): string = 
  let ic = open_in file in

  let parse_line(line: string): int = 
    let sign = match line.[0] with
      | 'L' -> -1
      | _ -> 1
    in
    let magnitude = int_of_string (Str.string_after line 1) in
    sign * magnitude
  in

  let rec loop(angle: int): int = 
    try
      let line = input_line ic in
      let angle_change = parse_line line in
      let new_angle = (angle + angle_change) mod 100 in
      match new_angle with
        | 0 -> 1 + loop new_angle
        | _ -> loop new_angle
      with End_of_file ->
        close_in ic;
        0
  in

  string_of_int (loop 50)
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
      let total_angle_change = parse_line line in

      let zeros = ref 0 in
      zeros := (abs total_angle_change) / 100;

      let remaining_angle_change = total_angle_change mod 100 in
      let new_angle = match (angle + remaining_angle_change) with
        | x when x < 0 -> x + 100
        | x when x >= 100 -> x - 100
        | x -> x 
      in

      (* print_string (string_of_int angle);
      print_string ", ";
      print_string (string_of_int total_angle_change);
      print_string ", ";
      print_string (string_of_int remaining_angle_change);
      print_string ", ";
      print_string (string_of_int new_angle);
      print_string "\n"; *)

      if new_angle = 0 then begin
        zeros := !zeros + 1;
        (* print_endline "adding A"; *)
      end
      else if angle != 0 && ((remaining_angle_change < 0 && new_angle > angle)
        || (remaining_angle_change > 0 && new_angle < angle)) 
      then begin
        (* print_endline "adding B"; *)
        zeros := !zeros + 1
      end;

      !zeros + loop new_angle
    with End_of_file ->
      close_in ic;
      0
  in

  string_of_int (loop 50)
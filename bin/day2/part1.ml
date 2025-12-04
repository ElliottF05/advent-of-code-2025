let file = "./bin/day2/part1.txt"

let parse_line (line: string) : int * int =
  match (String.split_on_char '-' line) with 
  | [x ; y] -> int_of_string x, int_of_string y
  | _ -> -1, -1

let rec repeat_str s n = 
  if n <= 0 then ""
  else s ^ (repeat_str s (n-1))

let rec get_half_digits (x : int) (is_lower : bool) : int =
  let x_str = string_of_int x in
  let num_digits = String.length x_str in
  match num_digits with
  | d when d mod 2 = 0 -> 
    int_of_string (Str.string_before x_str (num_digits / 2))
  | _d -> 
    let new_x = match is_lower with
    | true -> int_of_float (10.0 ** (float_of_int num_digits))
    | false -> int_of_float (10.0 ** (float_of_int (num_digits - 1))) - 1
    in
    get_half_digits new_x is_lower

let join_halves (half : int) : int =
  let half_str = string_of_int half in
  let joined_str = half_str ^ half_str in
  int_of_string joined_str

let rec count_ids_from (curr_half : int) (high_half : int) (low_limit : int) (high_limit : int) : int = 
  match curr_half > high_half with
  | true -> 0
  | false ->
    let joined = join_halves curr_half in
    match joined > high_limit with 
    | true -> 0
    | false -> 
      match joined < low_limit with 
      | true -> count_ids_from (curr_half + 1) high_half low_limit high_limit
      | false -> joined + count_ids_from (curr_half + 1) high_half low_limit high_limit
  

let count_all_ids (low : int) (high : int) : int =
  let low_half = get_half_digits low true in
  let high_half = get_half_digits high false in
  count_ids_from low_half high_half low high


let main (): string =
  let file_content = In_channel.with_open_text file In_channel.input_all in
  let ranges = String.split_on_char ',' file_content in
  let rec loop curr_ranges =
    match curr_ranges with 
    | [] -> 0
    | curr :: next -> 
      let low, high = parse_line curr in
      count_all_ids low high + loop next
  in

  string_of_int (loop ranges)
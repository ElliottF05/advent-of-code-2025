let file = "./bin/day2/part1.txt"

module IntSet = Set.Make (Int)

let rec pow10 (pow : int) = 
  if pow = 0 then 1
  else 10 * (pow10 (pow - 1))

let num_digits (x : int) : int = 
  String.length (string_of_int x)

let parse_line (line: string) : int * int =
  match (String.split_on_char '-' line) with 
  | [x ; y] -> int_of_string x, int_of_string y
  | _ -> -1, -1

let rec repeat_x (x : int) (reps : int) : int = 
  if reps = 0 then 
    0
  else 
    let pow = pow10 (num_digits x) in
    pow * (repeat_x x (reps - 1)) + x

let range_contains (range : (int * int)) (x : int) : bool = 
  let low, high = range in
  low <= x && x <= high

let ranges_contain (ranges : (int * int) list) (x : int) : bool = 
  List.fold_left (fun acc range -> acc || (range_contains range x)) false ranges

let rec find_ids_for_prefix (prefix : int) (count : int) (ranges : (int * int) list) (max : int) : int list =
  (* Printf.printf "prefix: %d\n" prefix; *)
  let x = repeat_x prefix count in
  if x > max then
    []
  else
    if ranges_contain ranges x then begin
      (* (Printf.printf "range contains %d\n" x); *)
      List.append [x] (find_ids_for_prefix prefix (count + 1) ranges max)
    end
    else
      find_ids_for_prefix prefix (count + 1) ranges max
  
let rec find_ids_starting_at (curr : int) (ranges : (int * int) list) (max : int) : int list =
  (* print_endline (string_of_int curr); *)
  if repeat_x curr 2 > max then
    []
  else
    List.append (find_ids_for_prefix curr 2 ranges max) (find_ids_starting_at (curr + 1) ranges max)


let main (): string =
  let file_content = In_channel.with_open_text file In_channel.input_all in
  let ranges = List.map (parse_line) (String.split_on_char ',' file_content) in
  let max = List.fold_left (fun acc range -> Int.max acc (snd range)) 0 ranges in
  let valid_id_list = find_ids_starting_at 1 ranges max in
  let valid_id_set = IntSet.of_list valid_id_list in

  (* print_endline (IntSet.fold (fun x acc -> acc ^ (string_of_int x) ^ ", ") valid_id_set ""); *)

  let sum = IntSet.fold (fun acc curr -> acc + curr) valid_id_set 0 in
  string_of_int sum

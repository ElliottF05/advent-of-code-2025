let print_list (elem_to_string : 'a -> string) (elems : 'a list) : unit = 
  print_string "[";
  let rec loop elems = 
    match elems with
    | [] -> ()
    | [x] -> print_string (elem_to_string x)
    | x :: next ->
      print_string (elem_to_string x);
      print_string "; ";
      loop next
  in
  loop elems;
  print_string "]\n";
;;

(* exclusive range from start to stop *)
let range start stop : int Seq.t = 
  let step = if start <= stop then 1 else -1 in
  let rec next i () = 
    if i = stop then
      Seq.Nil
    else
      Seq.Cons (i, next (i + step))
  in
  next start
;;

let get_rows_and_cols (arr : 'a array array) : (int * int) = 
  (Array.length arr, Array.length (Array.get arr 0))

let range_over_2d (rows : int) (cols : int) : ((int * int) Seq.t) =
  Seq.product (range 0 rows) (range 0 cols) (* i love this built-in cartesian product *)

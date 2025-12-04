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

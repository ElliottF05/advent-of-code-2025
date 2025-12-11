let () = 
    let start_time = Sys.time() in
    let answer = Day10.Part2.main () in
    let end_time = Sys.time() in
    Printf.printf "Answer: %s\n" answer;
    Printf.printf "Execution time: %f seconds\n" (end_time -. start_time);

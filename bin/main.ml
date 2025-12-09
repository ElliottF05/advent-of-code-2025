let () = 
    let start_time = Sys.time() in
    Printf.printf "Answer: %s\n" (Day9.Part2.main ());
    let end_time = Sys.time() in
    Printf.printf "Execution time: %f seconds\n" (end_time -. start_time);

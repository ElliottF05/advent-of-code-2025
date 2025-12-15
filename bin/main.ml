let () = 
    (* Run results for an individual day and part *)
    let start_time = Sys.time () in
    let answer = Day8.Part2.main () in
    let end_time = Sys.time () in
    Printf.printf "Answer: %s\n" answer;
    Printf.printf "Execution time: %f seconds\n" (end_time -. start_time);

    (* let start_time = Sys.time () in
    Printf.printf "day 1 answers: part1 = %s, part2 = %s\n" (Day1.Part1.main ()) (Day1.Part2.main ());
    Printf.printf "day 2 answers: part1 = %s, part2 = %s\n" (Day2.Part1.main ()) (Day2.Part2.main ());
    Printf.printf "day 3 answers: part1 = %s, part2 = %s\n" (Day3.Part1.main ()) (Day3.Part2.main ());
    Printf.printf "day 4 answers: part1 = %s, part2 = %s\n" (Day4.Part1.main ()) (Day4.Part2.main ());
    Printf.printf "day 5 answers: part1 = %s, part2 = %s\n" (Day5.Part1.main ()) (Day5.Part2.main ());
    Printf.printf "day 6 answers: part1 = %s, part2 = %s\n" (Day6.Part1.main ()) (Day6.Part2.main ());
    Printf.printf "day 7 answers: part1 = %s, part2 = %s\n" (Day7.Part1.main ()) (Day7.Part2.main ());
    Printf.printf "day 8 answers: part1 = %s, part2 = %s\n" (Day8.Part1.main ()) (Day8.Part2.main ());
    Printf.printf "day 9 answers: part1 = %s, part2 = %s\n" (Day9.Part1.main ()) (Day9.Part2.main ());
    Printf.printf "day 10 answers: part1 = %s, part2 = %s\n" (Day10.Part1.main ()) (Day10.Part2.main ());
    Printf.printf "day 11 answers: part1 = %s, part2 = %s\n" (Day11.Part1.main ()) (Day11.Part2.main ());
    Printf.printf "day 12 answers: part1 = %s\n"(Day12.Part1.main ());
    let end_time = Sys.time () in
    Printf.printf "Execution time: %f seconds\n" (end_time -. start_time); *)

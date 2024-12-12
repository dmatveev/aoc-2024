let () =
  let calc ic =
    match In_channel.input_line ic with
    | None -> ()
    | Some line ->
       let m = Aoc_2024.Dec09.disk_of_map_str line in
       let s1 = Aoc_2024.Dec09.checksum (Aoc_2024.Dec09.defrag m) in
       let s2 = Aoc_2024.Dec09.checksum (Aoc_2024.Dec09.defrag_whole m) in
       Printf.printf "%d %d\n" s1 s2 in

  In_channel.with_open_text "data/dec09.txt" calc;

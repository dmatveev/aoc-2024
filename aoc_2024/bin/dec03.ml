let () =
  let s = In_channel.with_open_text "data/dec03.txt" In_channel.input_all in
  print_endline (string_of_int (Aoc_2024.Dec03.run s).accum);
  print_endline (string_of_int (Aoc_2024.Dec03.run_guarded s).accum)

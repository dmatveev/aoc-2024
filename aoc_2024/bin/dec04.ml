let () =
  let mat = Aoc_2024.Common.load_matrix "data/dec04.txt" in
  Printf.printf "%d\n" (Aoc_2024.Dec04.count_xmas mat);
  Printf.printf "%d\n" (Aoc_2024.Dec04.count_x_mas mat)

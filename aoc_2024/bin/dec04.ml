let rec read_lines file =
  match In_channel.input_line file with
  | Some line -> line :: (read_lines file)
  | None -> []

let () =
  let ss = In_channel.with_open_text "data/dec04.txt" read_lines in
  let rows = List.length ss in
  let cols = String.length (List.hd ss) in
  let mat  = Array.make_matrix rows cols '.' in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      mat.(r).(c) <- (List.nth ss r).[c];
    done;
  done;
  Printf.printf "%d\n" (Aoc_2024.Dec04.count_xmas mat);
  Printf.printf "%d\n" (Aoc_2024.Dec04.count_x_mas mat)

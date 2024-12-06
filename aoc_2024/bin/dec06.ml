let () =
  let mat = Aoc_2024.Common.load_matrix "data/dec06.txt" in
  let (rows, cols) = Aoc_2024.Common.size_of_mat mat in
  let start = ref (0,0) in
  Aoc_2024.Common.iter_2d_f
    (fun r c x -> if List.mem x ['^';'>';'v';'<'] then start := (r,c) else ())
    mat;
  let out r c = r < 0 || r >= rows || c < 0 || c >= cols in

  let rot = function
    | '^' -> '>'
    | '>' -> 'v'
    | 'v' -> '<'
    | '<' -> '^'
    | _ -> raise (Failure "Unknown direction") in

  let next r c = function
    | '^' -> (r - 1, c)
    | '>' -> (r, c + 1)
    | 'v' -> (r + 1, c)
    | '<' -> (r, c - 1)
    | _ -> raise (Failure "Unknown direction") in

  let rec step d (this_r, this_c) =
    mat.(this_r).(this_c) <- 'X';
    let (wr, wc) = next this_r this_c d in
    if out wr wc then ()
    else match mat.(wr).(wc) with
         | '#' -> step (rot d) (this_r, this_c)
         | _ -> step d (wr, wc) in

  match !start with (r, c) -> step mat.(r).(c) !start;

  let xx = ref 0 in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      let m = mat.(r).(c) in
      if m = 'X' then xx := !xx + 1 else ();
    done;
  done;
  print_endline (string_of_int !xx)

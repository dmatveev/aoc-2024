let () =
  let m = Aoc_2024.Common.load_matrix "data/dec12.txt" in
  let (rows, cols) = Aoc_2024.Common.size_of_mat m in
  let out (r, c) = r < 0 || r >= rows || c < 0 || c >= cols in

  let score g (r, c) =
    let area = ref 0 in
    let peri = ref 0 in
    let marked = ref [] in
    let mark (r, c) = m.(r).(c) <- '.'; marked := (r, c)::!marked in
    let reset () = List.iter (fun (r, c) -> m.(r).(c) <- '#') !marked in
    let rec step (r, c) =
      if out (r, c) then peri := !peri + 1
      else match m.(r).(c) with | '.' -> ()
                                | x when x  = g -> area := !area + 1;
                                                   mark (r   ,  c    );
                                                   step (r - 1, c    );
                                                   step (r + 1, c    );
                                                   step (r    , c - 1);
                                                   step (r    , c + 1);
                                | _ -> peri := !peri + 1 in
    step (r, c); reset (); !area * !peri in

  let total = ref 0 in
  let process r c = function | '#' -> ()
                             |  g  -> total := !total + score g (r, c) in

  Aoc_2024.Common.iter_2d_f process m;
  Printf.printf "%d\n" !total

let () =
  let m = Aoc_2024.Common.load_matrix "data/dec10.txt" in
  let peaks = ref [] in
  let collect_peaks r c = function | '9' -> peaks := (r, c)::!peaks
                                   |  _  -> () in
  Aoc_2024.Common.iter_2d_f collect_peaks m;

  let mark_peak x (r, c) = m.(r).(c) <- x in
  let reset_peaks () = List.iter (mark_peak '9') !peaks in

  let (rows, cols) = Aoc_2024.Common.size_of_mat m in
  let out (r, c) = r < 0 || r >= rows || c < 0 || c >= cols in
  let succ c = Char.chr (Char.code c + 1) in

  let rec score n (r, c) f =
    if out (r, c) then 0 else
      match m.(r).(c) with
      | '9' when n = '9' -> f (r, c); 1
      |  x  when x = n ->
          let o = succ n in
          let dd = [-1,0; 1,0; 0,-1; 0,1] in
          List.fold_left (fun a (dr, dc) -> a + score o (r + dr, c + dc) f) 0 dd
      | _ -> 0 in

  let s = ref 0 in
  let t = ref 0 in
  let count r c = function
    | '0' -> s := !s + score '0' (r, c) (mark_peak '#'); reset_peaks ();
             t := !t + score '0' (r, c) (fun _ -> ());
    | _ -> () in
  Aoc_2024.Common.iter_2d_f count m;
  Printf.printf "%d\n%d\n" !s !t

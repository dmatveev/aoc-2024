type point = { r: int; c: int }
type beam = { o: point; d: point }

let rec pairs = function
  | [] -> []
  | _::[] -> []
  | x::xs -> List.map (fun y -> x, y) xs @ pairs xs

let l_r a b = if a.c < b.c then a, b else b, a
let t_b a b = if a.r < b.r then a, b else b, a

let antinodes a b =
  let (l, r) = l_r a b in
  let (t, b) = t_b a b in
  let (dc, dr) = r.c - l.c, b.r - t.r in
  let pt_tl = { c = l.c - dc; r = t.r - dr } in
  let pt_tr = { c = r.c + dc; r = t.r - dr } in
  let pt_bl = { c = l.c - dc; r = b.r + dr } in
  let pt_br = { c = r.c + dc; r = b.r + dr } in
  if l = t then pt_tl, pt_br (* \ *) else pt_tr, pt_bl (* / *)

let harmonic a b =
  let (l, r) = l_r a b in
  let (t, b) = t_b a b in
  let (dc, dr) = r.c - l.c, b.r - t.r in
  if l = t
  then { o = b; d = { r = -dr; c = -dc } },
       { o = t; d = { r =  dr; c =  dc } } (* \ *)
  else { o = b; d = { r = -dr; c =  dc } },
       { o = t; d = { r =  dr; c = -dc } } (* / *)

let () =
  let mat = Aoc_2024.Common.load_matrix "data/dec08.txt" in
  let (rows, cols) = Aoc_2024.Common.size_of_mat mat in
  let freqs = ref [] in
  let emitters = Hashtbl.create 1024 in

  let out r c = r < 0 || r >= rows || c < 0 || c >= cols in
  let reg r c = function | '.' -> ()
                         |  x  -> Hashtbl.add emitters x {r = r; c = c};
                                  freqs := x :: !freqs in
  Aoc_2024.Common.iter_2d_f reg mat;
  freqs := List.sort_uniq Char.compare !freqs;

  let ans = ref 0 in
  let anmap = Array.make_matrix rows cols '.' in

  let h_ans = ref 0 in
  let h_anmap = Array.make_matrix rows cols '.' in

  let put m r a =
    let modify a =
      match m.(a.r).(a.c) with
      | '#' -> ();
      |  _ -> m.(a.r).(a.c) <- '#'; r := !r + 1 in
    if out a.r a.c then () else modify a in

  let beam b =
    let rec set pt =
      if out pt.r pt.c then () else (
        put h_anmap h_ans pt;
        set { r = pt.r + b.d.r; c = pt.c + b.d.c }) in
    set b.o in

  let place x =
    let pps = pairs (Hashtbl.find_all emitters x) in
    List.iter (fun (a, b) ->
        let (an1, an2) = antinodes a b in put anmap ans an1; put anmap ans an2;
        let (b1, b2) = harmonic a b in beam b1; beam b2) pps in
  List.iter place !freqs;

  Printf.printf "%d\n%d\n" !ans !h_ans

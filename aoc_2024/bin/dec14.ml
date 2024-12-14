type point = { x: int; y: int }
type robot = { p: point; v: point }

let () =
  let load ic =
    let r px py dx dy = Some { p = {x = px; y = py }; v = {x = dx; y = dy} } in
    let get () = Scanf.bscanf ic "p=%d,%d v=%d,%d\n" r in
    let rec impl () =
      match (try get () with _-> None) with | None -> []
                                            | Some r -> r :: (impl ()) in
    impl () in
  let rs  = Aoc_2024.Common.scan_file "data/dec14.txt" load in
  let _W  = 101 in
  let _H  = 103 in
  let m   = Array.make_matrix _H _W 0 in
  let sat = Array.make_matrix _H _W 0 in

  let mark p = m.(p.y).(p.x) <- m.(p.y).(p.x) + 1 in

  let reset () =
    Aoc_2024.Common.loop_2d (fun r c -> m.(r).(c) <- 0; sat.(r).(c) <- 0) (_H, _W) in

  let integral () =
    let ivalue = function
      | (0, 0) -> m.(0).(0)
      | (0, c) -> m.(0).(c) + sat.(0).(c - 1)
      | (r, 0) -> m.(r).(0) + sat.(r - 1).(0)
      | (r, c) -> m.(r).(c) + sat.(r - 1).(c) + sat.(r).(c - 1) - sat.(r - 1).(c - 1) in
    Aoc_2024.Common.loop_2d (fun r c -> sat.(r).(c) <- ivalue (r, c))  (_H, _W) in

  let sum_of tl br =
    sat.(br.y).(br.x) +
      match tl with
      | {x = 0; y = 0} -> 0
      | {x = 0; y = _} -> (- sat.(tl.y - 1).(br.x))
      | {x = _; y = 0} -> (- sat.(br.y).(tl.x - 1))
      | _ -> sat.(tl.y - 1).(tl.x - 1) - sat.(tl.y - 1).(br.x) - sat.(br.y).(tl.x - 1) in

  let pos n r =
    let wrap x b = let m = x mod b in if m < 0 then b + m else m in
    { x = wrap (r.p.x + n * r.v.x) _W; y = wrap (r.p.y + n * r.v.y) _H } in

  List.iter (fun r -> pos 100 r |> mark) rs;
  integral ();

  let sfactor = let q1 = sum_of {y =        0; x =        0} {y = _H/2 - 1; x = _W/2 - 1} in
                let q2 = sum_of {y =        0; x = _W/2 + 1} {y = _H/2 - 1; x = _W   - 1} in
                let q3 = sum_of {y = _H/2 + 1; x =        0} {y = _H   - 1; x = _W/2 - 1} in
                let q4 = sum_of {y = _H/2 + 1; x = _W/2 + 1} {y = _H   - 1; x = _W   - 1} in
                q1 * q2 * q3 * q4 in
  Printf.printf "%d\n" sfactor;

  let participants = 0.6 in
  let threshold = 0.4 in
  let dw = 4 in
  let w = int_of_float (sqrt (participants *. float_of_int (List.length rs))) in
  let t' = int_of_float (threshold *. float_of_int (w * w)) in
  let (w_y, w_x) = (_H - w) / dw, (_W - w) / dw in
  let looks_like () =
    let w_count = ref 0 in
    for i = 0 to w_y - 1 do
      for j = 0 to w_x - 1 do
        let (t, l, b, r) = i*dw, j*dw, i*dw + w, j*dw + w in
        let qsum = sum_of {y = t; x = l} {y = b; x = r } in
        if qsum >= t' then (w_count := !w_count + 1) else ();
      done
    done;
    !w_count > 0 in

  let p r c = let x = m.(r).(c) in if x <> 0 then '#' else '.' in
  for s = 0 to _W * _H do
    reset ();
    List.iter (fun r -> pos s r |> mark) rs;
    integral ();
    if looks_like () then (Printf.printf "\nAt %d:\n" s;
                           for r = 0 to _H - 1 do
                             for c = 0 to _W - 1 do print_char (p r c) done;
                             print_newline ()
                           done)
    else ()
  done

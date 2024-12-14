type point = { x: int; y: int }

type record = { a: point; b: point; prize: point }

let det a b c d = a * d - b * c

let det_m r = det r.a.x     r.b.x     r.a.y     r.b.y
let det_a r = det r.prize.x r.b.x     r.prize.y r.b.y
let det_b r = det r.a.x     r.prize.x r.a.y     r.prize.y

let trivial r = let a's = (r.prize.x / r.a.x) in
                let b's = (r.prize.x / r.b.x) in
                if 3 * a's < b's then (a's, 0) else (0, b's)

let solution r = match (det_m r, det_a r, det_b r) with
  | (0, 0, 0) -> Some (trivial r)
  | (0, _, 0) -> None
  | (d, a, b) when a mod d <> 0 || b mod d <> 0 -> None
  | (d, a, b) -> Some (a / d, b / d)

let less_equal x = function | Some (a, b) when a <= x && b <= x -> Some (a, b)
                            | _ -> None

let cost_of = function | Some (a, b) -> Some (3 * a + b)
                       | _ -> None

let solution2 r = solution {r with prize = { x = r.prize.x + 10000000000000;
                                             y = r.prize.y + 10000000000000} }

let () =
  let load ic =
    let rcd ax ay bx by px py =
      { a = {x = ax; y = ay }; b = {x = bx; y = by}; prize = {x = px; y = py} } in
    let get () =
      Scanf.bscanf ic "Button A: X+%d, Y+%d\nButton B: X+%d, Y+%d\nPrize: X=%d, Y=%d\n" rcd in
    let adv () =
      try Scanf.bscanf ic "\n" true with _ -> false in
    let rec impl () = let r = get () in r::(if adv () then impl () else []) in
    impl () in
  let rs = Aoc_2024.Common.scan_file "data/dec13.txt" load in

  let upd r = function | Some c -> r := !r + c
                       | _ -> () in
  let total = ref 0 in
  let total_adj = ref 0 in

  List.iter (fun r -> solution  r |> less_equal 100 |> cost_of |> upd total    ) rs;
  List.iter (fun r -> solution2 r                   |> cost_of |> upd total_adj) rs;
  Printf.printf "%d\n%d\n" !total !total_adj

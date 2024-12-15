type point = { x: int; y: int }

let offset p (dr, dc) = { y = p.y + dr; x = p.x + dc }
let (@@) = offset

let load ic =
  let rec load_map () =
    match In_channel.input_line ic with
    | None -> []
    | Some s -> if String.length s > 0 then s :: (load_map ()) else [] in
  let rec load_moves () =
    match In_channel.input_line ic with
    | None -> []
    | Some s -> s :: (load_moves ()) in
  let map = load_map   () |> Aoc_2024.Common.matrix_of_lines in
  let mvs = load_moves () |> String.concat "" |> String.to_seq |> List.of_seq in
  map, mvs

let () =
  let (map, moves) = In_channel.with_open_text "data/dec15.txt" load in
  let now_at = ref {x = 0; y = 0} in
  Aoc_2024.Common.iter_2d_f (fun r c x -> if x = '@' then now_at := {x = c; y = r} else ()) map;

  let rec process = function | '<' -> ignore (try_move !now_at ( 0, -1));
                             | '>' -> ignore (try_move !now_at ( 0,  1));
                             | '^' -> ignore (try_move !now_at (-1,  0));
                             | 'v' -> ignore (try_move !now_at ( 1,  0));
                             |  _  -> ()
  and try_move this_p (dr, dc) =
    let next_p = this_p @@ (dr, dc) in
    match map.(next_p.y).(next_p.x) with
    | '.' -> move this_p next_p
    | 'O' -> if try_move next_p (dr, dc) then move this_p next_p else false
    |  _  -> false
  and move from_p to_p =
    let x = map.(from_p.y).(from_p.x) in
    map.(from_p.y).(from_p.x) <- '.';
    map.(to_p.y).(to_p.x) <- x;
    if x = '@' then now_at := to_p else ();
    true in
  List.iter process moves;

  let score = ref 0 in
  Aoc_2024.Common.iter_2d_f (fun r c -> function | 'O' -> score := !score + 100*r + c
                                                 | _ -> ())  map;
  Printf.printf "%d\n" !score

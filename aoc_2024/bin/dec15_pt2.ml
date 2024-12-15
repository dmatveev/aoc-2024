type point = { x: int; y: int }

let offset p (dr, dc) = { y = p.y + dr; x = p.x + dc }
let (@@) = offset

let front p dr = (p @@ (dr, -1)), (p @@ (dr,  0)), (p @@ (dr,  1))

let rec widen s = String.to_seq s |> w_impl |> String.of_seq
and w_impl s () = match s () with
  | Seq.Cons (c, seq) ->
     (match c with
      | 'O' -> Seq.Cons ('[' , (fun () -> Seq.Cons (']' , w_impl seq)))
      | '@' -> Seq.Cons ('@' , (fun () -> Seq.Cons ('.' , w_impl seq)))
      |  _  -> Seq.Cons ( c  , (fun () -> Seq.Cons ( c  , w_impl seq))))
  | s -> s

let load ic =
  let rec load_map () =
    match In_channel.input_line ic with
    | None -> []
    | Some s -> if String.length s > 0 then (widen s) :: (load_map ()) else [] in
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

  let move from_p to_p =
    let x = map.(from_p.y).(from_p.x) in
    map.(from_p.y).(from_p.x) <- '.';
    map.(to_p.y).(to_p.x) <- x;
    if x = '@' then now_at := to_p else ();
    true in

  let move_box from_p to_p =
    map.(  to_p.y).(  to_p.x    ) <- '[';
    map.(  to_p.y).(  to_p.x + 1) <- ']';
    map.(from_p.y).(from_p.x    ) <- '.';
    map.(from_p.y).(from_p.x + 1) <- '.' in

  let rec move_boxes this_p dr =
    let (next_p'', next_p, next_p') = front this_p dr in
    (match (map.(next_p.y).(next_p.x), map.(next_p'.y).(next_p'.x)) with
     | ('[', _ ) -> move_boxes next_p   dr
     | (']','.') -> move_boxes next_p'' dr
     | (']','[') -> move_boxes next_p'' dr; move_boxes next_p' dr
     | ('.','[') -> move_boxes next_p'  dr;
     | ('.','.') -> ()
     | _ -> raise (Failure "Unexpected case"));
    move_box this_p next_p in

  let rec can_move box_p dr =
    let (next_p'', next_p, next_p') = front box_p dr in
    match map.(next_p.y).(next_p.x) with
    | '[' -> can_move next_p   dr
    | ']' -> can_move next_p'' dr && can_step next_p'  dr
    | '.' -> can_step next_p'  dr
    | _ -> false
  and can_step this_p dr = match map.(this_p.y).(this_p.x) with
    | '[' -> can_move  this_p             dr
    | ']' -> can_move (this_p @@ (0, -1)) dr
    | '.' -> true
    |  _ -> false in

  let try_move_ver this_p dr =
    let (next_p'', next_p, _) = front this_p dr in
    match map.(next_p.y).(next_p.x) with
    | '[' when can_move next_p   dr -> move_boxes next_p   dr; ignore (move this_p next_p)
    | ']' when can_move next_p'' dr -> move_boxes next_p'' dr; ignore (move this_p next_p)
    | '.' -> ignore (move this_p next_p)
    |  _  -> () in
  let rec try_move_hor this_p dc =
    let next_p = this_p @@ (0, dc) in
    match map.(next_p.y).(next_p.x) with
    | '['
    | ']' -> if try_move_hor next_p dc then move this_p next_p else false
    | '.' -> move this_p next_p
    |  _  -> false in
  let process = function | '<' -> ignore (try_move_hor !now_at (-1));
                         | '>' -> ignore (try_move_hor !now_at ( 1));
                         | '^' -> ignore (try_move_ver !now_at (-1));
                         | 'v' -> ignore (try_move_ver !now_at ( 1));
                         |  _  -> () in
  List.iter process moves;

  let score = ref 0 in
  Aoc_2024.Common.iter_2d_f (fun r c -> function | '[' -> score := !score + 100*r + c
                                                 | _ -> ())  map;
  Printf.printf "%d\n" !score

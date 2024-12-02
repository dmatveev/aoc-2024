let load_lists_u chan =
  let group1 = ref [] in
  let group2 = ref [] in
  let continue = ref true in
  while !continue do
    match input_line chan with
    | x -> Scanf.sscanf x "%d %d" (fun a b ->
               group1 := a :: !group1;
               group2 := b :: !group2);
    | exception End_of_file ->
       continue := false
  done;
  !group1, !group2

let load_lists fname =
  let ic = open_in fname in
  let finally () = close_in ic in
  let work() = load_lists_u ic in
  Fun.protect ~finally work

let rec dist_sum g1 g2 =
  let error_diff () = raise (Failure "Lists of different lengths") in
  match (g2, g1) with
  | [], [] -> 0
  | (h1 :: gg1), (h2 :: gg2) -> abs(h1 - h2) + dist_sum gg1 gg2
  | [], _ -> error_diff ()
  | _, [] -> error_diff ()

let () =
  match (load_lists "data/dec01.txt") with (group1, group2) ->
    let sg1 = List.sort compare group1 in
    let sg2 = List.sort compare group2 in
    Printf.printf "%d\n" (dist_sum sg1 sg2)

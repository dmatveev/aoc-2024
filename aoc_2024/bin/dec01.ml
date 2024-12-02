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

let dist_sum g1 g2 =
  List.combine g1 g2
  |> List.map (fun (a, b) -> abs(a - b))
  |> List.fold_left (+) 0

let () =
  match (load_lists "data/dec01.txt") with (group1, group2) ->
    let sg1 = List.sort compare group1 in
    let sg2 = List.sort compare group2 in
    Printf.printf "%d\n" (dist_sum sg1 sg2)

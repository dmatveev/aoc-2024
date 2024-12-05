let load fname =
  let rules = Hashtbl.create 1024 in
  let updates = ref [] in

  let rec load_rules ic =
    match In_channel.input_line ic with
    | None -> ()
    | Some "" -> ()
    | Some line ->
       Scanf.sscanf line "%d|%d" (fun a b -> Hashtbl.add rules a b);
       load_rules ic in

  let rec load_updates ic =
    match In_channel.input_line ic with
    | None -> []
    | Some line ->
       let update = String.split_on_char ',' line |> List.map int_of_string in
       update :: (load_updates ic) in

  In_channel.with_open_text fname (fun ic ->
      load_rules ic; updates := load_updates ic);
  rules, !updates

let valid update rules =
  let rec valid_impl past uus = match (past, uus) with
    | (_, []) -> true
    | (p, u::us) -> let successors = Hashtbl.find_all rules u in
                    if List.exists (fun s -> List.mem s p) successors then false
                    else valid_impl (u::p) us in
  valid_impl [] update

let rec fix update rules = match update with
  | [] -> []
  | u::us -> let successors = Hashtbl.find_all rules u in
             let is_succ x = List.mem x successors in
             match (List.partition is_succ us) with (r, l) ->
               (fix l rules) @ u::(fix r rules)

let () =
  match load "data/dec05.txt" with (rules, updates) ->
    let sum = ref 0 in
    let sum_f = ref 0 in
    let mid_val uu = List.nth uu ((List.length uu) / 2) in

    let count uu = if valid uu rules
                   then sum := !sum + mid_val uu
                   else sum_f := !sum_f + mid_val (fix uu rules) in
    List.iter count updates;
    Printf.printf "%d\n%d\n" !sum !sum_f

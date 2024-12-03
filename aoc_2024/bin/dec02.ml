let str_to_list s = String.split_on_char ' ' s |> List.map int_of_string

let check a b = let d = b - a in d >= 1 && d <= 3

let rec safe_soft_past xs t p = match(xs, t) with
  | ([], _) -> true
  | (_::[], _) -> true
  | ((a::(b::_)), 0) when not (check a b) -> false
  | ((a::(b::ts)), t') when not (check a b) && t' > 0 ->
     (safe_soft_past (a::ts) (t-1) p) || (check p b && safe_soft_past (b::ts) (t-1) p)
  | ((a::(b::ts)), _) when check a b -> safe_soft_past (b::ts) t a
  | (_::_::_, _) -> raise (Failure "Invalid contract")

let rec safe_soft xs t = match xs with
  | [] -> true
  | _::[] -> true
  | a::(b::ts) ->
     if check a b then safe_soft_past (b::ts) t a
     else if t == 0 then false else safe_soft (b::ts) (t-1) || safe_soft (a::ts) (t-1)

let strict_report xs = safe_soft xs 0 || safe_soft (List.rev xs) 0
let soft_report xs = safe_soft xs 1 || safe_soft (List.rev xs) 1

let count_safe chan =
  let num_safe_strict = ref 0 in
  let num_safe_soft = ref 0 in
  let continue = ref true in
  let stop () = continue := false in
  while !continue do
    match input_line chan with
    | exception End_of_file -> stop ()
    | x -> match (str_to_list x) with
           | r when (strict_report r) -> num_safe_strict := 1 + !num_safe_strict;
           | r when (soft_report r) -> num_safe_soft := 1 + !num_safe_soft;
           | _ -> ()
    done;
  !num_safe_strict, !num_safe_soft

let () =
  let ic = open_in "data/dec02.txt" in
  let finally () = close_in ic in
  let work () = count_safe ic in
  match (Fun.protect ~finally work) with (strict, soft) ->
    Printf.printf "%d\n%d\n" strict (strict + soft)

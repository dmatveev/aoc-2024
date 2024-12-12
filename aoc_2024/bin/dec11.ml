let stones s = String.split_on_char ' ' s |> List.map int_of_string

let even_sized x = (Aoc_2024.Common.num_digits x) mod 2 = 0

let even_split x =
  let rec impl m = function | i when i <= 0 -> (x / m), (x mod m)
                            | i -> impl (m * 10) (i - 2) in
  impl 1 (Aoc_2024.Common.num_digits x)

let blink xs n h =
  let rec gen s i =
    match Hashtbl.find_opt h (s, i) with
    | Some x -> x
    | None -> let v = calc s i in Hashtbl.replace h (s, i) v; v
  and calc s i =
    if i = 0 then 1
    else match s with
         | 0 -> gen 1 (i - 1)
         | x when even_sized x -> let (l, r) = even_split x in
                                  gen l (i - 1) + gen r (i - 1)
         | x -> gen (2024 * x) (i - 1) in
  List.fold_left (fun a x -> a + gen x n) 0 xs

let () =
  let h = Hashtbl.create 1024 in
  let s = "3935565 31753 437818 7697 5 38 0 123" in
  let r = blink (stones s) 25 h in
  let m = blink (stones s) 75 h in
  Printf.printf "%d\n%d\n" r m

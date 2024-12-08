type equation = {
    test_value: int;
    operands: int list
  }

let eq_of_string str =
  let ic = Scanf.Scanning.from_string str in
  let tv = Scanf.bscanf ic "%d:" (fun x -> x) in
  let rec load_list () = match Scanf.bscanf ic " %d" (fun x -> x) with
    | exception End_of_file -> []
    | x -> x :: load_list() in
  { test_value = tv; operands = load_list () }

let rec num_digits = function
  | 0 -> 1
  | n when n < 0 -> num_digits (-1 * n)
  | n when n < 10 -> 1
  | n -> 1 + num_digits (n / 10)

let ccat a b =
  let rec impl a n = if n = 0 then a + b else impl (a * 10) (n - 1) in
  impl a (num_digits b)
let (++) = ccat

let test_eq eq =
  let rec impl acc = function
    | [] -> eq.test_value = acc
    | _ when acc > eq.test_value -> false
    | x::xs -> impl (acc + x) xs || impl (acc * x) xs in
  match eq.operands with
  | [] -> false
  | x::xs -> impl x xs

let test_eq_cat eq =
  let rec impl acc = function
    | [] -> eq.test_value = acc
    | _ when acc > eq.test_value -> false
    | x::xs -> impl (acc + x) xs || impl (acc * x) xs || impl (acc ++ x) xs in
  match eq.operands with
  | [] -> false
  | x::xs -> impl x xs

let () =
  let good_s = ref 0 in
  let good_s_cat = ref 0 in

  let rec load ic =
    match In_channel.input_line ic with
    | None -> ()
    | Some line ->
       let eq = eq_of_string line in
       if test_eq eq then good_s := !good_s + eq.test_value else ();
       if test_eq_cat eq then good_s_cat := !good_s_cat + eq.test_value else ();
       load ic in

  In_channel.with_open_text "data/dec07.txt" load;
  Printf.printf "%d\n%d\n" !good_s !good_s_cat

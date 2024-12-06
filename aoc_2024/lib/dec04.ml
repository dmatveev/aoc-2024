let count_xmas mat =
  let (rows, cols) = Common.size_of_mat mat in
  let count = ref 0 in

  let within r c = r >= 0 && r < rows && c >= 0 && c < cols in

  let rec try_follow x r c dr dc =
    let this_r = r + dr in
    let this_c = c + dc in
    if within this_r this_c && x = mat.(this_r).(this_c) then
      match x with
      | 'M' -> try_follow 'A' this_r this_c dr dc
      | 'A' -> try_follow 'S' this_r this_c dr dc
      | 'S' -> count := !count + 1
      |  _  -> () in

  let spread r c =
    let dd = [-1,0; 1,0; 0,-1; 0,1; -1,-1; 1,1; -1,1; 1,-1] in
    List.iter (function (dr, dc) -> try_follow 'M' r c dr dc) dd in

  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      if mat.(r).(c) = 'X' then spread r c else ()
    done;
  done;

  !count


let count_x_mas mat =
  let rows = Array.length mat in
  let cols = Array.length mat.(0) in
  let count = ref 0 in

  let within r c = r >= 0 && r < rows && c >= 0 && c < cols in

  let combos = ['M','S','M','S';
                'S','S','M','M';
                'M','M','S','S';
                'S','M','S','M'] in

  let test r c v = within r c && mat.(r).(c) = v in

  let matches r c (tl,tr,bl,br) =
    List.for_all (fun x -> x) [test (r-1) (c-1) tl;
                               test (r-1) (c+1) tr;
                               test (r+1) (c-1) bl;
                               test (r+1) (c+1) br] in

  let check r c = List.exists (matches r c) combos in

  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      if mat.(r).(c) = 'A' && check r c then count := !count + 1 else ()
    done;
  done;

  !count

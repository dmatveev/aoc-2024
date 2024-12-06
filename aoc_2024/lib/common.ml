let rec read_lines file =
  match In_channel.input_line file with
  | Some line -> line :: (read_lines file)
  | None -> []

let matrix_of_lines ss =
  let rows = List.length ss in
  let cols = String.length (List.hd ss) in
  let mat  = Array.make_matrix rows cols '.' in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      let x = (List.nth ss r).[c] in mat.(r).(c) <- x;
    done;
  done;
  mat

let load_matrix fname =
  In_channel.with_open_text fname read_lines |> matrix_of_lines

let size_of_mat m = Array.length m, Array.length m.(0)

let iter_2d f m =
  let (rows, cols) = size_of_mat m in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      f r c;
    done;
  done;
  ()

let iter_2d_f f m = iter_2d (fun r c -> f r c m.(r).(c)) m

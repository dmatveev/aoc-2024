val read_lines: in_channel -> string list

val matrix_of_lines: string list -> char array array

val load_matrix: string -> char array array

val size_of_mat: char array array -> int * int

val iter_2d: (int -> int -> unit) -> char array array -> unit

val iter_2d_f: (int -> int -> char -> unit) -> char array array -> unit

val loop_2d: (int -> int -> unit) -> int * int -> unit

val num_digits: int -> int

val scan_file : 'a. string -> (Scanf.Scanning.in_channel -> 'a) -> 'a

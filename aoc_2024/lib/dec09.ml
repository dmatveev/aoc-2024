type chunk_type = File of int | Free

type chunk = { t: chunk_type; size: int }

let is_file ch = (function | File _ -> true
                           | _ -> false) ch.t

let trim ch x = {ch with size = min ch.size x}

let disk_of_map xs =
  let dig c = Char.code c - Char.code '0' in
  let rec impl id = function
    | [] -> []
    | x::[] -> [{ t = File id; size = dig x }]
    | x::(f::xs) -> let file = { t = File id; size = dig x } in
                    let free = { t = Free; size = dig f } in
                    file :: (free :: impl (id + 1) xs) in
  impl 0 xs

let disk_of_map_str s = String.to_seq s |> List.of_seq |> disk_of_map

let checksum cs =
  let chunk_checksum base = function
    | { t = Free; size = _ } -> 0
    | { t = File i; size = sz } ->
       let idx = List.init sz (fun x -> base + x) in
       List.map (fun a -> a * i) idx |> List.fold_left (fun a b -> a + b) 0 in
  let rec impl cs' pos = match cs' with
    | [] -> 0
    | x::xs -> chunk_checksum pos x + impl xs (pos + x.size) in
  impl cs 0

let disk_size cs = List.fold_left (fun a c -> a + c.size) 0 cs

let used_size cs = List.filter is_file cs |> disk_size

let defrag cs =
  let result = ref [] in
  let written = ref 0 in
  let to_write = used_size cs in
  let trunc ch = trim ch (to_write - !written) in
  let write ch =
    written := ch.size + !written;
    let m () = match (!result, ch) with
      | ({ t = File i; size = x }::xs, { t = File j; size = y}) when i = j ->
         { t = File i; size = x + y } :: xs
      | (xs, _) -> ch :: xs in
    result := m () in
  let rec impl front back =
    if !written = to_write then ()
    else match (front, back) with
         | ( _, []) -> ()
         | ([],  _) -> ()
         | (head::fs, tail::bs) ->
            match (head.t, tail.t) with
            | (     _, Free  ) -> impl front bs
            | (File _, _     ) -> write (trunc head); impl fs back
            | (Free  , File _) ->
               let dsize = head.size - tail.size in
               match dsize with
               | 0 -> write tail; impl fs bs
               | _ when dsize > 0 -> let rem_free = trim head dsize in
                                     write tail; impl (rem_free::fs) bs
               | _ -> let new_chunk = trim tail head.size in
                      let rem_chunk = trim tail (abs dsize) in
                      write new_chunk; impl fs (rem_chunk::bs) in
  impl cs (List.rev cs);
  List.rev !result

let defrag_whole cs =
  let rec place ch = function
    | [] -> []
    | ch'::cs when ch = ch' -> ch'::cs
    | { t = Free; size = x}::cs when x = ch.size ->
       ch::(drop ch cs)
    | { t = Free; size = x}::cs when x > ch.size ->
       ch::({t = Free; size = x - ch.size}::(drop ch cs))
    | t::cs -> t::(place ch cs)
  and drop ch = function
    | [] -> []
    | ch'::cs when ch = ch' -> {t = Free; size = ch.size}::cs
    | t::cs -> t::(drop ch cs) in
  let rec impl disk_now = function
    | [] -> disk_now
    | b::bs when is_file b -> impl (place b disk_now) bs
    | _::bs -> impl disk_now bs in
  impl cs (List.rev cs)

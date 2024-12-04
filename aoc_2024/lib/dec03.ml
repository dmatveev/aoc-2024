type parse_state = Init | M | U | L | OpA | OpB

type process_state = {
    accum : int;
    last_a : int;
    last_b : int;
    pstate : parse_state
  }

let init_state = {accum = 0; last_a = 0; last_b = 0; pstate = Init}

let is_digit = function '0' .. '9' -> true | _ -> false

let cval c = Char.code c - Char.code '0'

let update st c = match (st.pstate, c) with
  | (Init, 'm') -> {st with pstate = M}
  | (M   , 'u') -> {st with pstate = U}
  | (U   , 'l') -> {st with pstate = L}
  | (L   , '(') -> {st with pstate = OpA}
  | (OpA ,  c ) when is_digit c && st.last_a < 1000 ->
     {st with last_a = st.last_a*10 + cval c}
  | (OpA , ',') -> {st with pstate = OpB}
  | (OpB ,  c ) when is_digit c && st.last_b < 1000 ->
     {st with last_b = st.last_b*10 + cval c}
  | (OpB , ')') -> {init_state with accum = st.accum + st.last_a*st.last_b }
  | _           -> {init_state with accum = st.accum}

let rec next st = function
  | x::xs -> next (update st x) xs
  | []    -> st

let run s = String.to_seq s |> List.of_seq |> next init_state

type guard_state = Init | D | O | N | Q | T | Open

type guard_mode = Do | Dont

let rec next_guarded gm gst st chars = match (gm, gst, chars) with
  | (_   ,    _,       []) -> st
  | (_   , Init, 'd' ::cs) -> next_guarded gm   D    st cs
  | (_   ,    D, 'o' ::cs) -> next_guarded gm   O    st cs
  | (Do  ,    O, 'n' ::cs) -> next_guarded gm   N    st cs
  | (Do  ,    N, '\''::cs) -> next_guarded gm   Q    st cs
  | (Do  ,    Q, 't' ::cs) -> next_guarded gm   T    st cs
  | (Do  ,    T, '(' ::cs) -> next_guarded gm   Open st cs
  | (Do  , Open, ')' ::cs) -> next_guarded Dont Init st cs
  | (Dont,    O, '(' ::cs) -> next_guarded gm   Open st cs
  | (Dont, Open, ')' ::cs) -> next_guarded Do   Init st cs
  | (Do  ,    _,  c  ::cs) -> next_guarded gm   Init (update st c) cs
  | (Dont,    _,  _  ::cs) -> next_guarded gm   Init st cs

let run_guarded s = String.to_seq s |> List.of_seq |> next_guarded Do D init_state

type parse_state

type process_state = {
    accum : int;
    last_a : int;
    last_b : int;
    pstate : parse_state
  }

val run: string -> process_state

val run_guarded: string -> process_state

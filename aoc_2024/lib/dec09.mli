type chunk_type = File of int | Free

type chunk = { t: chunk_type; size: int }

val disk_of_map_str: string -> chunk list

val checksum : chunk list -> int

val defrag: chunk list -> chunk list

val defrag_whole: chunk list -> chunk list


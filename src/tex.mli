type source
type csname = string
type 'a cfg

(** Building a config *)
val init : (string -> 'a) -> ('a list -> 'a) -> 'a cfg
val copy : 'a cfg -> 'a cfg
val register_cmd : 'a cfg -> csname -> (source -> 'a) -> unit
val register_env : 'a cfg -> csname -> (source -> 'b) -> ('b -> 'c cfg) -> ('b -> 'c -> 'a) -> unit

val cfg_raw : string cfg
val read_arg : 'a cfg -> source -> 'a
val read_opt : 'a cfg -> source -> 'a -> 'a
val read_item : 'a cfg -> csname -> source -> 'a

(** Parsing *)
type perror =
  | Unknown_command of csname
  | Unknown_environment of csname
  | Misplaced_end of csname
  | Unexpected_eof

val parse : 'a cfg -> string -> ('a, ('a * (int * perror) list)) result


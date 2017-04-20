exception End_of_stream

type source = string
type csname = string
type 'a cfg

(** Building a config *)
val init : (string -> 'a) -> ('a list -> 'a) -> 'a cfg
val copy : 'a cfg -> 'a cfg
val register_cmd : 'a cfg -> csname -> (source -> 'a) -> unit
val register_env : 'a cfg -> csname -> (source -> 'b) -> ('b -> 'c cfg) -> ('b -> 'c list -> 'a) -> unit

val cfg_raw : string cfg
val read_arg : 'a cfg -> source -> 'a
val read_opt : 'a cfg -> source -> 'a -> 'a

(** Parsing *)
val parse : 'a cfg -> source -> 'a


exception End_of_stream

type source = string
type 'a cfg

(** Building a config *)
val init : (string -> 'a) -> ('a list -> 'a) -> 'a cfg
val register_cmd : 'a cfg -> string -> (source -> 'a) -> unit

val read_arg : 'a cfg -> source -> 'a
val read_arg_raw : source -> string
val read_opt : 'a cfg -> source -> 'a -> 'a
val read_opt_raw : source -> string -> string

(** Parsing *)
val parse : 'a cfg -> source -> 'a


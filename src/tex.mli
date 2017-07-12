type source = string
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
(*type perror =
  | Unclosed_environment of int
  | Unclosed_group of int
  | Unclosed_option of int
  | Missing_argumnet of string * int
  | Unexpected_option of string * int
val parse : 'a cfg -> source -> ('a * ('a * string list)) result*)

val parse : 'a cfg -> source -> 'a


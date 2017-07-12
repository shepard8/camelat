(*
 * Camelat - Parsing LaTeX-like text in OCaml
 * Copyright (C) year  name of author
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

(**
 * Camelat works on text input. It is based on configurations, which
 * essentially associate actions to commands. Configurations are parameterized
 * by the output type that will be obtained when applied on a text input.
 *
 * Constructing configurations is relatively easy : Start by initializing an
 * empty configuration using [init], then add latex-like commands and
 * environments using register_cmd and register_env. While registering such
 * objects, one can use read_* functions to collect arguments, optional
 * arguments or item content from the input.
 *
 * Complete examples can be found either in the tests or in the examples
 * directories.
 *)

type 'a cfg
(** A configuration determines what behaviour is associated to each command and
 * environment *)

type source
type csname = string

(** {1 Building a config} *)

val init : (string -> 'a) -> ('a list -> 'a) -> 'a cfg
(** [init ftext fgroup] initializes a new configuration, applying [ftext] when
 * text is encountered, and using [fgroup] to group several parsed elements
 * into a single element. *)

val copy : 'a cfg -> 'a cfg
(** [copy cfg] allows you to fork a configuration if, for example, you want a
 * command to act differently in two distinct configurations while everything
 * else is kept unchanged. *)

val register_cmd : 'a cfg -> csname -> (source -> 'a) -> unit
(** [register_cmd cfg csname f] associates the action of [f] to the command
 * [csname] in [cfg]. You may want to have a look at {read_arg}, {read_opt} and
 * {read_item} to construct the [f] function. *)

val register_env : 'a cfg -> csname -> (source -> 'b) -> ('b -> 'c cfg) ->
  ('b -> 'c -> 'a) -> unit
(** [register_env cfg csname init subcfg f] associates the action of [f] to the
 * environment [csname] in [cfg]. You can retrieve arguments associated to the
 * environment (for example in "\\begin{enumerate}[b)] ...") within the [init]
 * function. You can chose the configuration in use within the environment
 * using [subcfg], which is parameterized by the result of [init]. *)


val read_arg : 'a cfg -> source -> 'a
(** [read_arg cfg s] reads an argument at the current position in the input.
 * Both braces-enclosed and single-character arguments are supported. *)

val read_opt : 'a cfg -> source -> 'a -> 'a
(** [read_opt cfg s default] reads an optional argument at the current position
 * in the input, defaulting to [default]. Arguments are brackets-enclosed text.
 * Any spaces before the opening bracket will be ignored. *)

val read_item : 'a cfg -> csname -> source -> 'a
(** [read_item cfg csname s] reads and parses input until either the [csname]
 * command or the end command is encountered. Its primary use is to parse LaTeX
 * lists. *)

val cfg_raw : string cfg
(** [cfg_raw] retrieves text with no formating at all. Add commands and
 * environments to this configuration at your own risk. [cfg_raw] is useful
 * when you have to retrieve arguments without processing them. For example as
 * in [let url = read_arg cfg_raw s]. *)

(** {1 Parsing} *)
type perror =
  | Unknown_command of csname
  | Unknown_environment of csname
  | Misplaced_end of csname
  | Unexpected_eof

val parse : 'a cfg -> string -> ('a, ('a * (int * perror) list)) result
(** [parse cfg input] parses [input] using configuration [cfg]. *)


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

(** This simple example illustrates how to use Camelat through a simple
 * configuration parsing text into text. *)

open Camelat

(* We start by initializing a configuration which will leave text untouched and
 * will just catenate groups of text. *)
let simplecfg = Cfg.init (fun s -> s) (fun l -> String.concat "" l)

(* Note that our configuration is already usable. Let's parse and print an
 * example: *)
let () =
  match Cfg.parse simplecfg "Hello, world!" with
  | Ok v -> print_endline v
  | Error (v, _) -> Printf.printf "%s (errors encountered)\n" v

(* Let's use a helper function that parses and prints for the next examples. *)
let pp s =
  match Cfg.parse simplecfg s with
  | Ok v -> print_endline v
  | Error (v, _) -> Printf.printf "%s (errors encountered)\n" v

let () =
  pp "I look like \\LaTeX"; (* LaTeX command is unknown *)
  pp "\\begin{test} \\end{test}"; (* test command is unknown *)
  pp "{Hello}"

(* Let's start easy with a command taking no arguments and printing something
 * useful. *)
let () =
  Cfg.register_cmd simplecfg "truth" (fun s -> "42")

let () =
  pp "\\truth";
  pp "The answer is \\truth!";
  pp "{\\truth}s"

(* We now register a command \\twice that will repeat its argument twice. *)
let () =
  Cfg.register_cmd simplecfg "twice" (fun s ->
    let argument = Cfg.read_arg simplecfg s in
    argument ^ argument
  )

let () =
  pp "I like \\twice{co}nuts.";
  pp "\\twice{\\twice{Hey! }}";
  pp "\\twice a"

(* Let's use optional arguments. The following function appends the character s
 * to its argument, unless some other string is given before as an optional
 * argument. *)
let () =
  Cfg.register_cmd simplecfg "plural" (fun s ->
    let append = Cfg.read_opt simplecfg s "s" in
    let argument = Cfg.read_arg simplecfg s in
    argument ^ append
  )

let () =
  pp "\\plural{bot}";
  pp "I'll need several \\plural{sheet}...";
  pp "\\plural[es]{watch}"

(* Okay, time to try out environments. *)
let () =
  Cfg.register_env simplecfg "imperative" (fun _ -> ()) (fun _ -> simplecfg) (fun () content ->
    String.map (function '.' -> '!' | c -> c) content
  )

let () =
  pp "\\begin{imperative}Go away.\\end{imperative}";
  pp "I asked you to \\begin{imperative}please. go. away\\end{imperative}!"

(* Environments can use parameters, and parse its content using another
 * configuration. *)
let () =
  Cfg.register_env simplecfg "subst"
  (fun s ->
    let a = match Cfg.read_arg simplecfg s with "" -> '.' | a -> a.[0] in
    let b = match Cfg.read_arg simplecfg s with "" -> '!' | b -> b.[0] in
    (a, b))
  (fun _ -> Cfg.cfg_text) (* Do not parse content *)
  (fun (a, b) content ->
    String.map (function x when x = a -> b | c -> c) content
  )

let () =
  pp "\\begin{subst}{i}{a}I drink a beer\\end{subst}";
  pp "\\begin{subst}{}{}\\truth.\\end{subst}" (* Note how \\truth is unknown *)


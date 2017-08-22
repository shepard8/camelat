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

(* TODO
   - Add inline/display support
   - Add config merge support
*)

module CfgMap = Map.Make (struct type t = string let compare = compare end)

type csname = string
type caret = int
type perror =
  | Unknown_command of csname
  | Unknown_environment of csname
  | Misplaced_end of csname
  | Unexpected_eof
type source = {
  text : string;
  mutable caret : caret;
  mutable errors : (caret * perror) list;
}
type 'a cfg = {
  mutable cmds : (source -> 'a) CfgMap.t;
  group : 'a list -> 'a;
  alterable : bool;
}

let r_empty = Str.regexp ""
let r_cmd = Str.regexp "\\\\\\([a-zA-Z]+\\)"
let r_text = Str.regexp "[^]\\\\{}]+"
let r_cbortext = Str.regexp "\\]\\|[^]\\\\{}]+"
let r_spaces = Str.regexp "[ \t\r\n]*"
let r_any = Str.regexp "."
let r_openbrace = Str.regexp "{"
let r_closebrace = Str.regexp "}"
let r_openbracket = Str.regexp "\\["
let r_closebracket = Str.regexp "\\]"
let r_envend env = Str.regexp ("\\\\end{" ^ env ^ "}")

(* Parsing *)

let updcaret s = s.caret <- Str.match_end ()
let adderror s e = s.errors <- (s.caret, e) :: s.errors

let read (cfg : 'a cfg) (s : source) : 'a =
  if Str.string_match r_cmd s.text s.caret then
    let csname = Str.matched_group 1 s.text in
    updcaret s;
    try (CfgMap.find csname (cfg.cmds)) s
    with Not_found ->
      adderror s (Unknown_command csname);
      (CfgMap.find "@text" (cfg.cmds)) s
  else if Str.string_match r_openbrace s.text s.caret then begin
    updcaret s;
    (CfgMap.find "@group" (cfg.cmds)) s
  end else begin (* will match either r_text or r_closebracket *)
    assert (Str.string_match r_cbortext s.text s.caret);
    updcaret s;
    (CfgMap.find "@text" (cfg.cmds)) s
  end

let rec read_until (cfg : 'a cfg) ?(r : Str.regexp option) (s : source) : 'a list =
  match r with
  | None when s.caret = String.length s.text -> []
  | Some _ when s.caret = String.length s.text ->
      adderror s Unexpected_eof;
      []
  | Some r when Str.string_match r s.text s.caret ->
      updcaret s;
      []
  | r ->
      let v = read cfg s in
      let l = read_until cfg ?r s in
      v :: l

let parse (cfg : 'a cfg) (text : string) =
  let s = { text = text ^ "}"; caret = 0; errors = [] } in
  let r = (CfgMap.find "@group" cfg.cmds) s in
  if s.errors = [] then Ok r else Error (r, List.rev s.errors)

(* Helpers *)

let skip_spaces s =
  ignore (Str.string_match r_spaces s.text s.caret);
  s.caret <- Str.match_end ()

let read_arg (cfg : 'a cfg) (s : source) : 'a =
  skip_spaces s;
  if Str.string_match r_openbrace s.text s.caret then begin
    updcaret s;
    (CfgMap.find "@group" cfg.cmds) s
  end else begin
    ignore (Str.string_match r_any s.text s.caret);
    updcaret s;
    (CfgMap.find "@text" cfg.cmds) s
  end

let read_opt (cfg : 'a cfg) (s : source) (default : 'a) : 'a =
  skip_spaces s;
  if Str.string_match r_openbracket s.text s.caret then begin
    updcaret s;
    (CfgMap.find "@opt" cfg.cmds) s
  end else default

let read_item (cfg : 'a cfg) (cmd : csname) (s : source) : 'a =
  skip_spaces s;
  let r = Str.regexp ("\\\\end{\\|\\\\" ^ cmd) in
  let content = read_until cfg ~r s in
  s.caret <- Str.match_beginning ();
  cfg.group content

(* Populating the config *)

let register_cmd (cfg : 'a cfg) (cmd : string) (f : source -> 'a) : unit =
  if cfg.alterable then
    cfg.cmds <- CfgMap.add cmd f cfg.cmds

let register_env (cfg : 'a cfg) (env : string) (init : source -> 'b) (subcfg : 'b -> 'c cfg) (f : 'b -> 'c -> 'a) : unit =
  if cfg.alterable then
    cfg.cmds <- CfgMap.add ("env@" ^ env) (fun (s : source) ->
      let v = init s in
      let subcfg = subcfg v in
      let content = read_until subcfg ~r:(r_envend env) s in
      f v (subcfg.group content)
    ) cfg.cmds

let inalterable cfg = { cfg with alterable = false }

let cfg_text =
  let cfg = { cmds = CfgMap.empty; group = String.concat ""; alterable = true } in
  register_cmd cfg "@text" (fun s -> Str.matched_string s.text);
  register_cmd cfg "@group" (fun s -> cfg.group (read_until cfg ~r:r_closebrace s));
  register_cmd cfg "@opt" (fun s -> cfg.group (read_until cfg ~r:r_closebracket s));
  inalterable cfg

let init (ftext : string -> 'a) (fgroup : 'a list -> 'a) =
  let cfg = { cmds = CfgMap.empty; group = fgroup; alterable = true } in
  register_cmd cfg "@text" (fun s -> ftext (Str.matched_string s.text));
  register_cmd cfg "@group" (fun s -> fgroup (read_until cfg ~r:r_closebrace s));
  register_cmd cfg "@opt" (fun s -> fgroup (read_until cfg ~r:r_closebracket s));
  register_cmd cfg "begin" (fun s ->
    let e = read_arg cfg_text s in
    try (CfgMap.find ("env@" ^ e) cfg.cmds) s
    with Not_found ->
      adderror s (Unknown_environment e);
      ftext ("\\begin{" ^ e ^ "}")
  );
  register_cmd cfg "end" (fun s ->
    let e = read_arg cfg_text s in
    adderror s (Misplaced_end e);
    ftext ("\\end{" ^ e ^ "}")
  );
  cfg

let copy cfg = { cfg with cmds = cfg.cmds } (* CHECK ME *)

let cfg_int = inalterable (init int_of_string (List.fold_left (+) 0))

let cfg_float = inalterable (init float_of_string (List.fold_left (+.) 0.))


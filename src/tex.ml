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
  group : 'a list -> 'a
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

  (* TODO restrict if cfg_raw *)
let register_cmd (cfg : 'a cfg) (cmd : string) (f : source -> 'a) : unit =
  cfg.cmds <- CfgMap.add cmd (fun (s : source) ->
    f s
  ) cfg.cmds

  (* TODO restrict if cfg_raw *)
let register_env (cfg : 'a cfg) (env : string) (init : source -> 'b) (subcfg : 'b -> 'c cfg) (f : 'b -> 'c -> 'a) : unit =
  cfg.cmds <- CfgMap.add ("env@" ^ env) (fun (s : source) ->
    let v = init s in
    let subcfg = subcfg v in
    let content = read_until subcfg ~r:(r_envend env) s in
    f v (subcfg.group content)
  ) cfg.cmds

let cfg_raw =
  let cfg = { cmds = CfgMap.empty; group = String.concat "" } in
  register_cmd cfg "@text" (fun s -> Str.matched_string s.text);
  register_cmd cfg "@group" (fun s -> cfg.group (read_until cfg ~r:r_closebrace s));
  register_cmd cfg "@opt" (fun s -> cfg.group (read_until cfg ~r:r_closebracket s));
  cfg

let init (ftext : string -> 'a) (fgroup : 'a list -> 'a) =
  let cfg = { cmds = CfgMap.empty; group = fgroup } in
  register_cmd cfg "@text" (fun s -> ftext (Str.matched_string s.text));
  register_cmd cfg "@group" (fun s -> fgroup (read_until cfg ~r:r_closebrace s));
  register_cmd cfg "@opt" (fun s -> fgroup (read_until cfg ~r:r_closebracket s));
  register_cmd cfg "begin" (fun s ->
    let e = read_arg cfg_raw s in
    try (CfgMap.find ("env@" ^ e) cfg.cmds) s
    with Not_found ->
      adderror s (Unknown_environment e);
      ftext ("\\begin{" ^ e ^ "}")
  );
  register_cmd cfg "end" (fun s ->
    let e = read_arg cfg_raw s in
    adderror s (Misplaced_end e);
    ftext ("\\end{" ^ e ^ "}")
  );
  cfg

let copy cfg = { cfg with cmds = cfg.cmds } (* CHECK ME *)


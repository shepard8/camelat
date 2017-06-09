(* TODO
   - Add environment support
   - Add inline/display support
   - Add unit tests
   - Add Eliom default config
   - Add config merge support
*)

exception End_of_stream

module CfgMap = Map.Make (struct type t = string let compare = compare end)

type source = string
type csname = string
type 'a cfg = {
  mutable cmds : (source -> int -> ('a * int)) CfgMap.t;
  group : 'a list -> 'a
}

let r_empty = Str.regexp ""
let r_cmd = Str.regexp "\\\\\\([a-zA-Z]+\\)"
let r_text = Str.regexp "[^]\\\\{}]+"
let r_spaces = Str.regexp "[ \t\r\n]*"
let r_arg = Str.regexp "{\\([^}]*\\)}"
let r_opt = Str.regexp "\\[\\([^]]*\\)\\]"
let r_any = Str.regexp "."
let r_openbrace = Str.regexp "{"
let r_closebrace = Str.regexp "}"
let r_openbracket = Str.regexp "\\["
let r_closebracket = Str.regexp "\\]"
let r_envend env = Str.regexp ("\\\\end{" ^ env ^ "}")


(* Parsing *)

let read (cfg : 'a cfg) (s : source) (i : int) : 'a * int =
  if Str.string_match r_cmd s i then
    try
      let f = CfgMap.find (Str.matched_group 1 s) (cfg.cmds) in
      let i = Str.match_end () in
      f s i
    with Not_found ->
      let i = Str.match_end () in
      let f = CfgMap.find "@text" (cfg.cmds) in
      f s i
  else if Str.string_match r_openbrace s i then
    let f = CfgMap.find "@group" (cfg.cmds) in
    let i = Str.match_end () in
    f s i
  else if Str.string_match r_text s i || Str.string_match r_closebracket s i then
    (* Those cases have to be handled in separate regexes for end of options to
     * be detected *)
    let f = CfgMap.find "@text" (cfg.cmds) in
    let i = Str.match_end () in
    f s i
  else assert false

let rec read_until (cfg : 'a cfg) ?(r : Str.regexp option) (s : source) (i : int) : 'a list * int =
  match r with
  | None when i = String.length s -> ([], i)
  | Some r when Str.string_match r s i -> ([], Str.match_end ())
  | r ->
      let (v, i) = read cfg s i in
      let (l, i) = read_until cfg ?r s i in
      (v :: l, i)

let parse (cfg : 'a cfg) (s : source) =
  let f = CfgMap.find "@group" (cfg.cmds) in
  fst (f (s ^ "}") 0)

(* Helpers *)
(* The following function must be "safe" w.r.t. Str.match_end (). *)
(*let read_env_content*)

let read_arg (cfg : 'a cfg) (s : source) : 'a =
  ignore (Str.string_match r_spaces s (Str.match_end ()));
  let i = Str.match_end () in
  if Str.string_match r_openbrace s i then
    let f = CfgMap.find "@group" (cfg.cmds) in
    let i = Str.match_end () in
    fst (f s i)
  else if Str.string_match r_any s i then
    let f = CfgMap.find "@text" (cfg.cmds) in
    fst (f s i)
  else assert false

let read_opt (cfg : 'a cfg) (s : source) (default : 'a) : 'a =
  let i = Str.match_end () in
  ignore (Str.string_match r_spaces s i);
  if Str.string_match r_openbracket s (Str.match_end ()) then
    let f = CfgMap.find "@opt" (cfg.cmds) in
    let i = Str.match_end () in
    fst (f s i)
  else begin
    assert (Str.string_match r_empty s i); (* Reset Str.match_end pointer *)
    default
  end


(* Populating the config *)

let cfg_raw =
  let cfg = { cmds = CfgMap.empty; group = String.concat "" } in
  cfg.cmds <- CfgMap.add "@text" (fun s i ->
    let t = Str.matched_string s in
    (t, i)
  ) (cfg.cmds);
  cfg.cmds <- CfgMap.add "@group" (fun s i ->
    let (l, i) = read_until cfg ~r:r_closebrace s i in
    (String.concat "" l, i)
  ) (cfg.cmds);
  cfg.cmds <- CfgMap.add "@opt" (fun s i ->
    let (l, i) = read_until cfg ~r:r_closebracket s i in
    (String.concat "" l, i)
  ) (cfg.cmds);
  cfg

let init (ftext : string -> 'a) (fgroup : 'a list -> 'a) =
  let cfg = { cmds = CfgMap.empty; group = fgroup } in
  cfg.cmds <- CfgMap.add "@text" (fun s i ->
    let t = Str.matched_string s in
    (ftext t, i)
  ) (cfg.cmds);
  cfg.cmds <- CfgMap.add "@group" (fun s i ->
    let (l, i) = read_until cfg ~r:r_closebrace s i in
    (fgroup l, i)
  ) (cfg.cmds);
  cfg.cmds <- CfgMap.add "@opt" (fun s i ->
    let (l, i) = read_until cfg ~r:r_closebracket s i in
    (fgroup l, i)
  ) (cfg.cmds);
  cfg.cmds <- CfgMap.add "begin" (fun s i ->
    let e = read_arg cfg_raw s in
    try
      let f = CfgMap.find ("env@" ^ e) (cfg.cmds) in
      f s i
    with Not_found -> (ftext ("\\begin{" ^ e ^ "}"), Str.match_end ())
  ) (cfg.cmds);
  cfg.cmds <- CfgMap.add "end" (fun s i ->
    let e = read_arg cfg_raw s in
    (ftext ("\\end{" ^ e ^ "}"), Str.match_end ())
  ) (cfg.cmds);
  cfg

let copy cfg = { cfg with cmds = cfg.cmds }

  (* TODO restrict if cfg_raw *)
let register_cmd (cfg : 'a cfg) (cmd : string) (f : source -> 'a) : unit =
  cfg.cmds <- CfgMap.add cmd (fun (s : source) (i : int) ->
    let v = f s in
    (v, Str.match_end ())
  ) (cfg.cmds)

  (* TODO restrict if cfg_raw *)
let register_env (cfg : 'a cfg) (env : string) (init : source -> 'b) (subcfg : 'b -> 'c cfg) (f : 'b -> 'c -> 'a) : unit =
  cfg.cmds <- CfgMap.add ("env@" ^ env) (fun (s : source) (i : int) ->
    let v = init s in
    let i = Str.match_end () in
    let subcfg = subcfg v in
    let (content, i) = read_until subcfg ~r:(r_envend env) s i in
    (f v (subcfg.group content), i)
  ) (cfg.cmds)


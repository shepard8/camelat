exception End_of_stream

module CfgMap = Map.Make (struct type t = string let compare = compare end)

type source = string
type 'a cfg = (source -> int -> ('a * int)) CfgMap.t

let r_cmd = Str.regexp "\\\\\\([a-zA-Z]+\\)"
let r_text = Str.regexp "\\(\\\\{\\|\\\\}\\|[^\\\\{}]\\)+"
let r_spaces = Str.regexp "[ \t\r\n]*"
let r_arg = Str.regexp "{\\([^}]*\\)}"
let r_any = Str.regexp "."
let r_openbrace = Str.regexp "{"
let r_closebrace = Str.regexp "}"


(* Parsing *)

let read (cfg : 'a cfg) (s : source) (i : int) : 'a * int =
  if Str.string_match r_cmd s i then
    try
      let f = CfgMap.find (Str.matched_group 1 s) cfg in
      let i = Str.match_end () in
      f s i
    with Not_found ->
      let i = Str.match_end () in
      let f = CfgMap.find "@text" cfg in
      f s i
  else if Str.string_match r_openbrace s i then
    let f = CfgMap.find "@group" cfg in
    let i = Str.match_end () in
    f s i
  else if Str.string_match r_text s i then
    let f = CfgMap.find "@text" cfg in
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

(* Helpers *)
(* All the following functions must be "safe" w.r.t. Str.match_end (). *)
(*let read_env_content
let read_arg
let read_opt
let read_raw_arg
let read_raw_opt*)

let read_arg_raw (s : source) : string =
  ignore (Str.string_match r_spaces s (Str.match_end ()));
  let i = Str.match_end () in
  if Str.string_match r_arg s i then
    Str.matched_group 1 s
  else if Str.string_match r_any s i then
    Str.matched_string s
  else assert false

let read_arg (cfg : 'a cfg) (s : source) : 'a list =
  ignore (Str.string_match r_spaces s (Str.match_end ()));
  let i = Str.match_end () in
  if Str.string_match r_openbrace s i then
    let f = CfgMap.find "@group" cfg in
    let i = Str.match_end () in
    [fst (f s i)]
  else if Str.string_match r_any s i then
    let f = CfgMap.find "@text" cfg in
    [fst (f s i)]
  else assert false


(* Populating the config *)

let register_text (cfg : 'a cfg) (f : string -> 'a) : 'a cfg =
  CfgMap.add "@text" (fun (s : source) (i : int) ->
    let t = Str.matched_string s in
    (f t, i)
  ) cfg

let register_group (cfg : 'a cfg) (f : 'a list -> 'a) : 'a cfg =
  CfgMap.add "@group" (fun (s : source) (i : int) ->
    let (l, i) = read_until cfg ~r:r_closebrace s i in
    (f l, i)
  ) cfg

let register_cmd (cfg : 'a cfg) (cmd : string) (f : source -> 'a) : 'a cfg =
  CfgMap.add cmd (fun (s : source) (i : int) ->
    let v = f s in
    (v, Str.match_end ())
  ) cfg


(* Test *)
let () =
  let cfg = CfgMap.empty in
  let cfg = register_text cfg (fun s -> "\"" ^ s ^ "\"") in
  let cfg = register_group cfg (fun l -> String.concat "" l) in
  let cfg = register_cmd cfg "test" (fun s -> "[test cmd]") in
  let cfg = register_cmd cfg "verb" (fun s ->
    let t = read_arg_raw s in
    "<i>" ^ t ^ "</i>"
  ) in
  let cfg = register_cmd cfg "i" (fun s ->
    let t = read_arg cfg s in
    "<i>" ^ String.concat "" t ^ "</i>"
  ) in
  let s = "coucou \n\\test\\blabla {coucou}  coucou \\verb {coucou} \\verb ijk \\i abc \\i{abc} \\i {abc} ???" in
  let (l, _) = read_until cfg s 0 in
  List.iter print_endline l


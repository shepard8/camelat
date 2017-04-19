(* TODO
   - Add environment support
   - Add inline/display support
   - Add unit tests
   - Add Eliom default config
*)

exception End_of_stream

module CfgMap = Map.Make (struct type t = string let compare = compare end)

type source = string
type csname = string
type 'a cfg = (source -> int -> ('a * int)) CfgMap.t ref

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


(* Parsing *)

let read (cfg : 'a cfg) (s : source) (i : int) : 'a * int =
  if Str.string_match r_cmd s i then
    try
      let f = CfgMap.find (Str.matched_group 1 s) (!cfg) in
      let i = Str.match_end () in
      f s i
    with Not_found ->
      let i = Str.match_end () in
      let f = CfgMap.find "@text" (!cfg) in
      f s i
  else if Str.string_match r_openbrace s i then
    let f = CfgMap.find "@group" (!cfg) in
    let i = Str.match_end () in
    f s i
  else if Str.string_match r_text s i || Str.string_match r_closebracket s i then
    (* Those cases have to be handled in separate regexes for end of options to
     * be detected *)
    let f = CfgMap.find "@text" (!cfg) in
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
  let f = CfgMap.find "@group" (!cfg) in
  fst (f (s ^ "}") 0)

(* Helpers *)
(* The following function must be "safe" w.r.t. Str.match_end (). *)
(*let read_env_content*)

let read_arg (cfg : 'a cfg) (s : source) : 'a =
  ignore (Str.string_match r_spaces s (Str.match_end ()));
  let i = Str.match_end () in
  if Str.string_match r_openbrace s i then
    let f = CfgMap.find "@group" (!cfg) in
    let i = Str.match_end () in
    fst (f s i)
  else if Str.string_match r_any s i then
    let f = CfgMap.find "@text" (!cfg) in
    fst (f s i)
  else assert false

let read_opt (cfg : 'a cfg) (s : source) (default : 'a) : 'a =
  let i = Str.match_end () in
  ignore (Str.string_match r_spaces s i);
  if Str.string_match r_openbracket s (Str.match_end ()) then
    let f = CfgMap.find "@opt" (!cfg) in
    let i = Str.match_end () in
    fst (f s i)
  else begin
    assert (Str.string_match r_empty s i); (* Reset Str.match_end pointer *)
    default
  end


(* Populating the config *)

let rec init ftext fgroup =
  let cfg = ref CfgMap.empty in
  cfg := CfgMap.add "@text" (fun s i ->
    let t = Str.matched_string s in
    (ftext t, i)
  ) (!cfg);
  cfg := CfgMap.add "@group" (fun s i ->
    let (l, i) = read_until cfg ~r:r_closebrace s i in
    (fgroup l, i)
  ) (!cfg);
  cfg := CfgMap.add "@opt" (fun s i ->
    let (l, i) = read_until cfg ~r:r_closebracket s i in
    (fgroup l, i)
  ) (!cfg);
  cfg := CfgMap.add "begin" (fun s i ->
    let e = read_arg (craw ()) s in
    try
      let f = CfgMap.find ("env@" ^ e) (!cfg) in
      f s i
    with Not_found -> (ftext ("\\begin{" ^ e ^ "}"), Str.match_end ())
  ) (!cfg);
  cfg := CfgMap.add "end" (fun s i ->
    let e = read_arg (craw ()) s in
    (ftext ("\\end{" ^ e ^ "}"), Str.match_end ())
  ) (!cfg);
  cfg
and craw () = init (fun s -> s) (String.concat "")

let cfg_raw = craw ()



let register_cmd (cfg : 'a cfg) (cmd : string) (f : source -> 'a) : unit =
  cfg := CfgMap.add cmd (fun (s : source) (i : int) ->
    let v = f s in
    (v, Str.match_end ())
  ) (!cfg)
  
(*
let register_env (cfg : 'a cfg) (env : string) ?init ?subcfg f : unit =
  if ! CfgMap.mem "begin" (!cfg) then begin
    cfg := CfgMap.add "begin" (fun (s : source) (i : int) ->
      let e = read_arg cfg_raw s in
      try
        let f = CfgMap.find ("env@" ^ e) (!cfg) in
        f s (Str.match_end ())
      with Not_found -> 
      if CfgMap.mem ("env@" ^ e) (!cfg) then
        
    ) (!cfg)
  end;
  cfg := CfgMap.add "begin" (fun (s : source) (i : int) ->
    ignore (Str.string_match r_empty i);
    let e = read_arg_raw cfg s in
    if e = env then let v = f source in (v, Str.match_end ())
    else fbegin source i
  ) (!cfg)
*)

(* Test *)

let cfgtest = init (fun s -> "\"" ^ s ^ "\"") (String.concat "")

let () =
  register_cmd cfgtest "test" (fun s -> "[test cmd]");
  register_cmd cfgtest "verb" (fun s ->
    let t = read_arg cfg_raw s in
    "<i>" ^ t ^ "</i>"
  );
  register_cmd cfgtest "i" (fun s ->
    let t = read_arg cfgtest s in
    "<i>" ^ t ^ "</i>"
  );
  register_cmd cfgtest "b" (fun s ->
    let o = read_opt cfg_raw s "test" in
    let a = read_arg cfgtest s in
    let o' = read_opt cfgtest s "XXX" in
    "<b>" ^ o ^ a ^ o' ^ "</b>"
  );
  register_cmd cfgtest "c" (fun s ->
    let o = read_opt cfgtest s "aoeu" in
    "<c>" ^ o ^ "</c>"
  )

let () =
  let s = "cou[co]u[ \n\\test\\blabla {coucou}  coucou \\verb {coucou} \\verb ijk \\i abc \\i{abc} \\i {abc} \\b a \\b [bla] b [Y] \\b b [y] ??? \\c [x] bla \\c \\begin{test} blabla \\end{test}" in
  let output = parse cfgtest s in
  print_endline output


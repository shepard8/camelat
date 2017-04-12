exception End_of_stream

type source = string

let r_cmd = Str.regexp "\\\\\\([a-zA-Z]+\\)"
let r_text = Str.regexp "\\(\\\\{\\|\\\\}\\|[^\\\\{}]\\)*"
let r_spaces = Str.regexp "[ \t\r\n]*"
let r_arg = Str.regexp "{\\([^}]*\\)}"
let r_any = Str.regexp "."

type 'a cfg = (string * (source -> int -> ('a * int))) list

let read (cfg : 'a cfg) (s : source) (i : int) : 'a * int =
  if Str.string_match r_cmd s i then
    try
      let f = List.assoc (Str.matched_group 1 s) cfg in
      let i = Str.match_end () in
      f s i
    with Not_found ->
      let f = List.assoc "text" cfg in
      let i = Str.match_end () in
      f s i
  else if Str.string_match r_text s i then
    let f = List.assoc "text" cfg in
    let i = Str.match_end () in
    f s i
  else raise End_of_stream

let rec read_until (cfg : 'a cfg) ?(r : Str.regexp option) (s : source) (i : int) : 'a list * int =
  match r with
  | None when i = String.length s -> ([], i)
  | Some r when Str.string_match r s i -> ([], i)
  | r ->
      let (v, i) = read cfg s i in
      let (l, i) = read_until cfg ?r s i in
      (v :: l, i)

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
  else if Str.string_match r_cmd s i then
    Str.matched_string s
  else if Str.string_match r_any s i then
    Str.matched_string s
  else assert false

let register_text (cfg : 'a cfg) (f : string -> 'a) : 'a cfg =
  ("text", fun (s : source) (i : int) ->
    let t = Str.matched_string s in
    (f t, i)) :: cfg

let register_cmd (cfg : 'a cfg) (cmd : string) (f : source -> 'a) : 'a cfg =
  (cmd, fun (s : source) (i : int) ->
    let v = f s in
    (v, Str.match_end ())) :: cfg

(*
let register_cmd cfg cmd n opts f =
  (cmd, fun s i ->
    let optionals = List.fold_left (
      fun (acc, i) opt -> match read_opt opt s i with
      | None -> (opt :: acc, i)
      | Some (o, i) -> (o :: acc, i)
    ) ([], i) opts in
    let rec aux_args (acc, i) = function
      | 0 -> (acc, i)
      | n ->
          let (arg, i) = read_arg s i in
          aux_args (arg :: acc, i) (n - 1)
    in
    let (args, i) = aux_args (optionals, n - List.length opts) in
    (f args, i)) :: cfg
*)

let () =
  let cfg = [] in
  let cfg = register_text cfg (fun s -> "\"" ^ s ^ "\"") in
  let cfg = register_cmd cfg "test" (fun s -> "[test cmd]") in
  let cfg = register_cmd cfg "i" (fun s ->
    let t = read_arg_raw s in
    "<i>" ^ t ^ "</i>"
  ) in
  let s = "coucou \n\\test\\blabla \\i{coucou} \\i coucou \\i \\coucou ???" in
  let (l, _) = read_until cfg s 0 in
  List.iter print_endline l


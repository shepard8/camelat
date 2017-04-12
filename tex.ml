exception End_of_stream

let r_cmd = Str.regexp "\\\\\\([a-zA-Z]+\\)"
let r_text = Str.regexp "\\(\\\\{\\|\\\\}\\|[^\\\\{}]\\)*"

type 'a cfg = (string * (string -> int -> 'a * int)) list

let read (cfg : 'a cfg) (s : string) (i : int) : 'a * int =
  if Str.string_match r_cmd s i then
    try
      let f = List.assoc (Str.matched_group 1 s) cfg in
      f s i
    with Not_found ->
      let f = List.assoc "text" cfg in
      f s i
  else if Str.string_match r_text s i then
    let f = List.assoc "text" cfg in
    f s i
  else raise End_of_stream

(*let rec read_until (cfg : 'a cfg) (r : Str.regexp) (s : string) (i : int) : 'a list * int =*)

let register_text (cfg : 'a cfg) (f : string -> 'a) : 'a cfg =
  ("text", fun s i ->
    let t = Str.matched_string s in
    (f t, Str.match_end ())) :: cfg

let register_cmd (cfg : 'a cfg) (cmd : string) (f : unit -> 'a) : 'a cfg =
  (cmd, fun s i ->
    (f (), i)) :: cfg
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
  let cfg = register_cmd cfg "test" (fun () -> "[test cmd]") in
  print_endline (fst (read cfg "coucou \n\\test" 0));
  print_endline (fst (read cfg "\\test coucou" 0));
  print_endline (fst (read cfg "\\blabla test" 0))

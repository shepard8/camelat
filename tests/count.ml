open TestSimple

(* Far from optimal (fast exp, tail-rec, check for overflows), I know ... *)
let rec power p x = match p with
| x when x <= 0 -> 1
| i -> x * power (p - 1) x

let cfg_count = Tex.init String.length (List.fold_left ( + ) 0)
let cfg_list = Tex.init (fun _ -> [0]) List.concat
let () =
  Tex.register_cmd cfg_count "double" (fun s ->
    2 * (Tex.read_arg cfg_count s)
  );
  Tex.register_cmd cfg_count "truth" (fun s -> 10);
  Tex.register_cmd cfg_count "truth" (fun s -> 42);
  Tex.register_cmd cfg_count "power" (fun s ->
    let p = int_of_string (Tex.read_opt Tex.cfg_raw s "2") in
    let v = Tex.read_arg cfg_count s in
    power p v
  );
  Tex.register_env cfg_count "cancel"
  (fun _ -> ())
  (fun () -> cfg_count)
  (fun () _ -> 0);
  Tex.register_env cfg_count "altdouble"
  (fun s -> int_of_string (Tex.read_arg Tex.cfg_raw s))
  (fun v ->
    let cfg' = Tex.copy cfg_count in
    Tex.register_cmd cfg' "double" (fun s ->
      let w = Tex.read_arg cfg' s in
      v * w
    );
    cfg')
  (fun _ c -> c);
  Tex.register_env cfg_count "lit"
  (fun _ -> ())
  (fun () ->
    let cfg' = Tex.init (fun s -> s) (fun l ->
      string_of_int (List.fold_left (fun acc x -> acc + int_of_string x) 0 l))
    in
    Tex.register_cmd cfg' "double" (fun s ->
      let v = Tex.read_arg cfg' s in
      Printf.sprintf "%d" (2 * int_of_string v)
    );
    cfg'
  )
  (fun _ c -> print_endline c; int_of_string c);
  Tex.register_env cfg_count "max"
  (fun _ -> ())
  (fun () -> cfg_list)
  (fun () l -> List.fold_left max 0 l);
  Tex.register_cmd cfg_list "it" (fun s -> [Tex.read_item cfg_count "it" s])

let testslist = [
  (0, "", "Empty text");
  (5, "abcde", "Only text");
  (48, "abc{\\truth}abc", "Using a command");
  (8, "\\double{abcd}", "One argument");
  (15, "1234\\double{abcd}abc", "One argument, surrounding text");
  (2, "\\double a", "Argument without braces");
  (7, "abc\\double bcd", "Argument without braces, surrounding text");
  (15, "abc\\power{abc}abc", "Optional argument (not given)");
  (33, "abc\\power[3]{abc}abc", "Optional argument (given)");
  (6, "abc\\begin{cancel}abc\\end{cancel}abc", "Environment");
  (114, "abc{\\truth}abc\\begin{altdouble}{4}abc\\double{bla}abc\\end{altdouble}abc{\\truth}abc",
  "Environment redefining things");
  (42, "abc\\begin{lit}36\\end{lit}abc",
  "Environment using sub-config with a different type");
  (15, "\\begin{lit}3\\double{6}\\end{lit}",
  "Command in an environment using sub-config with a different type");
  (0, "\\begin{max}\\end{max}", "Empty items list");
  (10, "coucou\\begin{max}\\end{max}test",
  "Empty items list with surrounding text");
  (10, "\\begin{max}\\it abc\\it coucou\\it coucoutest\\end{max}",
  "Simple items list");
  (42, "\\begin{max}\\it \\truth\\it \\double{abcde}\\end{max}",
  "Simple items list with commands");
  (42, "\\begin{max}\\it \\begin{max}\\it abc\\it{abcde}\\end{max}\\it \\begin{max}\\it \\truth\\it \\double{abcde}\\end{max}\\end{max}",
  "Test nested lists");
]

let count = List.length testslist
let tests () = List.iter (fun (n, s, c) ->
  let r = Tex.parse cfg_count s in
  match Tex.parse cfg_count s with
  | Ok n' -> is n' n c
  | Error (n', errors) ->
      print_endline (c ^ " :");
      List.iter (fun (i, m) ->
        Printf.printf "\t%d : %s\n" i (match m with
        | Tex.Unknown_command csname -> "Unknown command " ^ csname
        | Tex.Unknown_environment csname -> "Unknown environment " ^ csname
        | Tex.Misplaced_end csname -> "Misplaced end " ^ csname
        | Tex.Unexpected_eof -> "Unexpected EOF"
      )) errors;
      is n' n c
) testslist


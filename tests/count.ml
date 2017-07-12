open TestSimple

(* Far from optimal (fast exp, tail-rec, check for overflows), I know ... *)
let rec power p x = match p with
| x when x <= 0 -> 1
| i -> x * power (p - 1) x

let cfg_count = Cfg.init String.length (List.fold_left ( + ) 0)
let cfg_list = Cfg.init (fun _ -> [0]) List.concat
let () =
  Cfg.register_cmd cfg_count "double" (fun s ->
    2 * (Cfg.read_arg cfg_count s)
  );
  Cfg.register_cmd cfg_count "truth" (fun s -> 10);
  Cfg.register_cmd cfg_count "truth" (fun s -> 42);
  Cfg.register_cmd cfg_count "power" (fun s ->
    let p = (Cfg.read_opt Cfg.cfg_int s 2) in
    let v = Cfg.read_arg cfg_count s in
    power p v
  );
  Cfg.register_env cfg_count "cancel"
  (fun _ -> ())
  (fun () -> cfg_count)
  (fun () _ -> 0);
  Cfg.register_env cfg_count "altdouble"
  (fun s -> Cfg.read_arg Cfg.cfg_int s)
  (fun v ->
    let cfg' = Cfg.copy cfg_count in
    Cfg.register_cmd cfg' "double" (fun s ->
      let w = Cfg.read_arg cfg' s in
      v * w
    );
    cfg')
  (fun _ c -> c);
  Cfg.register_env cfg_count "lit"
  (fun _ -> ())
  (fun () ->
    let cfg' = Cfg.init (fun s -> s) (fun l ->
      string_of_int (List.fold_left (fun acc x -> acc + int_of_string x) 0 l))
    in
    Cfg.register_cmd cfg' "double" (fun s ->
      let v = Cfg.read_arg cfg' s in
      Printf.sprintf "%d" (2 * int_of_string v)
    );
    cfg'
  )
  (fun _ c -> print_endline c; int_of_string c);
  Cfg.register_env cfg_count "max"
  (fun _ -> ())
  (fun () -> cfg_list)
  (fun () l -> List.fold_left max 0 l);
  Cfg.register_cmd cfg_list "it" (fun s -> [Cfg.read_item cfg_count "it" s])

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
  (42, "abc{\\double{abc}}abc\\begin{altdouble}{4}abc\\double{bla}abc\\end{altdouble}abc{\\double{abc}}abc",
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
  match Cfg.parse cfg_count s with
  | Ok n' -> is n' n c
  | Error (n', errors) ->
      print_endline (c ^ " :");
      List.iter (fun (i, m) ->
        Printf.printf "\t%d : %s\n" i (match m with
        | Cfg.Unknown_command csname -> "Unknown command " ^ csname
        | Cfg.Unknown_environment csname -> "Unknown environment " ^ csname
        | Cfg.Misplaced_end csname -> "Misplaced end " ^ csname
        | Cfg.Unexpected_eof -> "Unexpected EOF"
      )) errors;
      is n' n c
) testslist


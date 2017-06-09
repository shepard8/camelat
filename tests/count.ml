open TestSimple

(* Far from optimal (fast exp, tail-rec, check for overflows), I know ... *)
let rec power p x = match p with
| x when x <= 0 -> 1
| i -> x * power (p - 1) x

let cfg_count = Tex.init String.length (List.fold_left ( + ) 0)
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
  (fun _ c -> print_endline c; int_of_string c)

let count = 10
let tests () =
  is 5 (Tex.parse cfg_count "abcde") "Only text";
  is 45 (Tex.parse cfg_count "{\\truth}abc") "Using a command";
  is 12 (Tex.parse cfg_count "1234\\double{abcd}") "One argument";
  is 4 (Tex.parse cfg_count "a\\double bc") "Argument without braces";
  is 15 (Tex.parse cfg_count "abc\\power{abc}abc") "Optional argument (not given)";
  is 33 (Tex.parse cfg_count "abc\\power[3]{abc}abc") "Optional argument (given)";
  is 6 (Tex.parse cfg_count "abc\\begin{cancel}abc\\end{cancel}abc") "Environment";
  is (30+84) (
    Tex.parse cfg_count
    "abc{\\truth}abc\\begin{altdouble}{4}abc\\double{bla}abc\\end{altdouble}abc{\\truth}abc"
  ) "Environment redefining things";
  is 42 (
    Tex.parse cfg_count "abc\\begin{lit}36\\end{lit}abc"
  ) "Environment using sub-config with a different type";
  is 15 (
    Tex.parse cfg_count "\\begin{lit}3\\double{6}\\end{lit}"
  ) "Command in an environment using sub-config with a different type"


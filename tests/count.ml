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
  Tex.register_cmd cfg_count "fortytwo" (fun s -> 42);
  Tex.register_cmd cfg_count "power" (fun s ->
    let p = int_of_string (Tex.read_opt Tex.cfg_raw s "2") in
    let v = Tex.read_arg cfg_count s in
    power p v
  )

let count = 6
let tests () =
  is 5 (Tex.parse cfg_count "abcde") "Only text";
  is 45 (Tex.parse cfg_count "{\\fortytwo}abc") "Using a command";
  is 12 (Tex.parse cfg_count "1234\\double{abcd}") "One argument";
  is 4 (Tex.parse cfg_count "a\\double bc") "Argument without braces";
  is 15 (Tex.parse cfg_count "abc\\power{abc}abc") "Optional argument (not given)";
  is 33 (Tex.parse cfg_count "abc\\power[3]{abc}abc") "Optional argument (given)"


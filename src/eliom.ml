open Eliom_content.Html.D

type pwi = Html_types.phrasing_without_interactive
type pwl = Html_types.phrasing_without_label
type p = Html_types.phrasing
type f5 = Html_types.flow5

let cfg_pwi : pwi elt list_wrap Tex.cfg = Tex.init (fun s -> [pcdata s]) List.concat
let cfg_pwl : pwl elt list_wrap Tex.cfg = Tex.init (fun s -> [pcdata s]) List.concat
let cfg_p : p elt list_wrap Tex.cfg = Tex.init (fun s -> [pcdata s]) List.concat
let cfg_f5 : f5 elt list_wrap Tex.cfg = Tex.init (fun s -> [pcdata s]) List.concat

let reg_wrap cfg name f sub =
  Tex.register_cmd cfg name (fun s -> [f (Tex.read_arg sub s)])

(* Bold, italic, underline, strike *)
let reg_style cfg name style sub =
  reg_wrap cfg name (fun a -> span ~a:[a_style style] a) sub

let () =
  let styles = [
    ("bold", "font-weight: bold;");
    ("italic", "font-style: italic;");
    ("underline", "text-decoration: underline;");
    ("strike", "text-decoration: line-through;")
  ] in
  List.iter (fun (n, s) -> reg_style cfg_pwi n s cfg_pwi) styles;
  List.iter (fun (n, s) -> reg_style cfg_pwl n s cfg_p) styles;
  List.iter (fun (n, s) -> reg_style cfg_p n s cfg_p) styles;
  List.iter (fun (n, s) -> reg_style cfg_f5 n s cfg_p) styles

(* Color *)
let reg_color cfg sub =
  Tex.register_cmd cfg "color" (fun s ->
    let color = Tex.read_arg Tex.cfg_raw s in
    let arg = Tex.read_arg sub s in
    [span ~a:[a_style ("color: " ^ color ^ ";")] arg])

let () =
  reg_color cfg_pwi cfg_pwi;
  reg_color cfg_pwl cfg_p;
  reg_color cfg_p cfg_p;
  reg_color cfg_f5 cfg_p

(* Link *)
let reg_link cfg =
  Tex.register_cmd cfg "link" (fun s ->
    let url = Tex.read_opt Tex.cfg_raw "" s in
    if url = "" then
      let arg = Tex.read_opt Tex.cfg_raw "" s in
      [ Raw.a ~a:[a_href (uri_of_string (fun () -> arg))] [pcdata arg] ]
    else
      let arg = Tex.read_arg cfg_pwi s in
      [ Raw.a ~a:[a_href (uri_of_string (fun () -> url))] arg ]
  )

let () =
  reg_link cfg_pwl;
  reg_link cfg_p

let () =
  Tex.register_cmd cfg_f5 "link" (fun s ->
    let url = Tex.read_opt Tex.cfg_raw "" s in
    if url = "" then
      let arg = Tex.read_opt Tex.cfg_raw "" s in
      [ Raw.a ~a:[a_href (uri_of_string (fun () -> arg))] [pcdata arg] ]
    else
      let arg = (Tex.read_arg cfg_pwi s :> Html_types.flow5_without_interactive elt list_wrap) in
      [ Raw.a ~a:[a_href (uri_of_string (fun () -> url))] arg ]
  )


  (*

let f_image = ("image", fun s ->
  let alt = Tex.read_opt Tex.cfg_raw "" s in
  let alt = if alt = "" then "user image" else alt in
  let arg = Tex.read_arg Tex.cfg_raw s in
  let src = Xml.uri_of_string arg in
  [ img ~alt ~src () ]
)

let f_section = cmd_enclose "section" h3 cfg_p
let f_subsection = cmd_enclose "subsection" h4 cfg_p
let f_subsubsection = cmd_enclose "subsubsection" h5 cfg_p

let f_size sub = ("size", fun s ->
  let size = Tex.read_arg Tex.cfg_raw s in
  let arg = Tex.read_arg sub s in
  [ span ~a:[a_style ("font-size: " ^ size ^ ";")] arg ])

let c_full = [
  f_italic cfg_p;
  f_bold cfg_p;
  f_underline cfg_p;
  f_link;
  f_image;
  f_color cfg_p;
  f_section; f_subsection; f_subsubsection;
  f_size cfg_p;
]
let c_phrasing = [
  f_italic cfg_p;
  f_bold cfg_p;
  f_underline cfg_p;
  f_link;
  f_image;
  f_color cfg_p;
  f_size cfg_p;
]
let c_coint = [
  f_italic cfg_pwi;
  f_bold cfg_pwi;
  f_underline cfg_pwi;
  f_image;
  f_color cfg_pwi;
  f_size cfg_pwi;
]

let () =
  List.iter (fun (csname, f) -> Tex.register_cmd cfg_f5 csname f) c_full;
  List.iter (fun (csname, f) -> Tex.register_cmd cfg_p csname f) c_phrasing;
  List.iter (fun (csname, f) -> Tex.register_cmd cfg_pwi csname f) c_coint
(*
let env_enclose name e =
  Tex.register_env cfg_f5 name (fun _ -> ()) (fun () -> cfg_f5) (fun () c -> e c)
let env_stylize name style = env_enclose name (fun c -> [div ~a:[a_style style] c])

let () =
  env_stylize "left" "text-align: left;";
  env_stylize "right" "text-align: right;";
  env_stylize "center" "text-align: center;";
  env_stylize "lfloat" "float: left;";
  env_stylize "rfloat" "float: right;"

let secret_id = ref 0
let newsecretid () =
  incr secret_id;
  string_of_int (!secret_id)

let () =
  Tex.register_env cfg_f5 "secret"
  (fun s ->
    let labelcontent = Tex.read_opt cfg_p [pcdata "Secret (cliquez pour afficher/cacher)"] s in
    (labelcontent, newsecretid ()))
  (fun _ -> cfg_f5)
  (fun (labelcontent, secret_id) content -> [div ~a:[a_class ["secret"]] [
    label ~a:[a_label_for secret_id] labelcontent;
    input ~a:[a_id secret_id; a_class ["secretcb"]; a_input_type `Checkbox] ();
    div ~a:[a_class ["secretdiv"]] content
  ]])
  *)
*)

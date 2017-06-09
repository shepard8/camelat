open Eliom_content.Html.D

type full = Html_types.flow5
type phrasing = Html_types.phrasing
type coint = Html_types.phrasing_without_interactive

let cfg_coint : coint elt list Tex.cfg = Tex.init (fun s -> [pcdata s]) List.concat

let cfg_phrasing : phrasing elt list Tex.cfg = Tex.init (fun s -> [pcdata s]) List.concat

let cfg_full : full elt list Tex.cfg = Tex.init (fun s -> [pcdata s]) List.concat

let cmd_enclose name e sub = (name, fun s -> [e (Tex.read_arg sub s)])
let cmd_stylize name style sub = cmd_enclose name (span ~a:[a_style style]) sub

let f_newline = ("newline", fun s -> [br ()])

let f_italic sub = cmd_stylize "italic" "font-style: italic;" sub
let f_bold sub = cmd_stylize "bold" "font-weight: bold;" sub
let f_underline sub = cmd_stylize "underline" "text-decoration: underline;" sub
let f_strike sub = cmd_stylize "strike" "text-decoration: line-through;" sub
let f_color sub = ("color", fun s ->
  let color = Tex.read_arg Tex.cfg_raw s in
  let arg = Tex.read_arg sub s in
  [ span ~a:[a_style ("color: " ^ color ^ ";")] arg ])

let f_link = ("link", fun s ->
  let url = Tex.read_opt Tex.cfg_raw "" s in
  if url = "" then
    let arg = Tex.read_opt Tex.cfg_raw "" s in
    [ Raw.a ~a:[a_href (uri_of_string (fun () -> arg))] [pcdata arg] ]
  else
    let arg = Tex.read_arg cfg_coint s in
    [ Raw.a ~a:[a_href (uri_of_string (fun () -> url))] arg ]
)

let f_image = ("image", fun s ->
  let alt = Tex.read_opt Tex.cfg_raw "" s in
  let alt = if alt = "" then "user image" else alt in
  let arg = Tex.read_arg Tex.cfg_raw s in
  let src = Xml.uri_of_string arg in
  [ img ~alt ~src () ]
)

let f_section = cmd_enclose "section" h3 cfg_phrasing
let f_subsection = cmd_enclose "subsection" h4 cfg_phrasing
let f_subsubsection = cmd_enclose "subsubsection" h5 cfg_phrasing

let f_size sub = ("size", fun s ->
  let size = Tex.read_arg Tex.cfg_raw s in
  let arg = Tex.read_arg sub s in
  [ span ~a:[a_style ("font-size: " ^ size ^ ";")] arg ])

type tfull = string * (string -> full elt list)
type tphrasing = string * (string -> phrasing elt list)
type tcoint = string * (string -> coint elt list)

let c_full = [
  f_newline;
  f_italic cfg_phrasing;
  f_bold cfg_phrasing;
  f_underline cfg_phrasing;
  (f_link : tphrasing :> tfull);
  f_image;
  f_color cfg_phrasing;
  f_section; f_subsection; f_subsubsection;
  f_size cfg_phrasing;
]
let c_phrasing = [
  f_newline;
  f_italic cfg_phrasing;
  f_bold cfg_phrasing;
  f_underline cfg_phrasing;
  f_link;
  f_image;
  f_color cfg_phrasing;
  f_size cfg_phrasing;
]
let c_coint = [
  f_newline;
  f_italic cfg_coint;
  f_bold cfg_coint;
  f_underline cfg_coint;
  f_image;
  f_color cfg_coint;
  f_size cfg_coint;
]

let () =
  List.iter (fun (csname, f) -> Tex.register_cmd cfg_full csname f) c_full;
  List.iter (fun (csname, f) -> Tex.register_cmd cfg_phrasing csname f) c_phrasing;
  List.iter (fun (csname, f) -> Tex.register_cmd cfg_coint csname f) c_coint

let env_enclose name e =
  Tex.register_env cfg_full name (fun _ -> ()) (fun () -> cfg_full) (fun () c -> e c)
let env_stylize name style = env_enclose name (fun c -> [div ~a:[a_style style] c])

let () =
  env_stylize "left" "text-align: left;";
  env_stylize "right" "text-align: right;";
  env_stylize "center" "text-align: center;";
  env_stylize "lfloat" "float: left;";
  env_stylize "rfloat" "float: right;"


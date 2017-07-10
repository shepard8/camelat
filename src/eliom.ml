open Eliom_content.Html.D

type 'a cfg = 'a Tex.cfg

let r_nl = Str.regexp "$"
let text s =
  let parts = Str.split (Str.regexp "$") s in
  let parts = List.rev_map pcdata parts in
  List.fold_left (fun acc t -> t :: br () :: acc) [List.hd parts] (List.tl parts)

type pwi = Html_types.phrasing_without_interactive
type pwl = Html_types.phrasing_without_label
type p = Html_types.phrasing
type f5 = Html_types.flow5

let cfg_pwi : pwi elt list_wrap cfg = Tex.init text List.concat
let cfg_pwl : pwl elt list_wrap cfg = Tex.init text List.concat
let cfg_p : p elt list_wrap cfg = Tex.init text List.concat
let cfg_f5 : f5 elt list_wrap cfg = Tex.init text List.concat

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
    [ span ~a:[a_style ("color: " ^ color ^ ";")] arg ]
  )

let () =
  reg_color cfg_pwi cfg_pwi;
  reg_color cfg_pwl cfg_p;
  reg_color cfg_p cfg_p;
  reg_color cfg_f5 cfg_p

(* Size *)
(* A sole number is considered a percentage. Otherwise we assume the unit is
 * given. *)
let reg_size cfg sub =
  Tex.register_cmd cfg "size" (fun s ->
    let size = Tex.read_arg Tex.cfg_raw s in
    let size = try
      ignore (int_of_string size);
      size ^ "%"
    with
    | Failure "int_of_string" -> size
    in
    let arg = Tex.read_arg sub s in
    [ span ~a:[a_style ("font-size: " ^ size ^ ";")] arg ]
  )

let () =
  reg_size cfg_pwi cfg_pwi;
  reg_size cfg_pwl cfg_p;
  reg_size cfg_p cfg_p;
  reg_size cfg_f5 cfg_p

(* Sectionning *)
let () =
  reg_wrap cfg_f5 "section" h3 cfg_p;
  reg_wrap cfg_f5 "subsection" h4 cfg_p;
  reg_wrap cfg_f5 "subsubsection" h5 cfg_p

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
      let arg = (Tex.read_arg cfg_pwi s :
        pwi elt list_wrap :>
        Html_types.flow5_without_interactive elt list_wrap
      ) in
      [ Raw.a ~a:[a_href (uri_of_string (fun () -> url))] arg ]
  )

(* Alignment *)
let reg_align where =
  Tex.register_env cfg_f5 where (fun s -> ()) (fun () -> cfg_f5)
  (fun () content -> [div ~a:[a_style ("text-align: " ^ where)] content])

let () =
  reg_align "left";
  reg_align "right";
  reg_align "center"

(* Floating *)
let reg_float where =
  Tex.register_env cfg_f5 ("f" ^ where) (fun s -> ()) (fun () -> cfg_f5)
  (fun () content -> [div ~a:[a_style ("float: " ^ where)] content])

let () =
  reg_float "left";
  reg_float "right";
  Tex.register_cmd cfg_f5 "clearer" (fun s ->
    [ div ~a:[a_style "clear: both;"] [] ]
  )

(* Image *)
let reg_image cfg =
  Tex.register_cmd cfg "image" (fun s ->
    let title = Tex.read_opt Tex.cfg_raw "User image" s in
    let url = Tex.read_arg Tex.cfg_raw s in
    let src = Xml.uri_of_string url in
    let alt = Tex.read_arg Tex.cfg_raw s in
    [ img ~alt ~src ~a:[a_title title] () ]
  )

let () =
  reg_image cfg_f5;
  reg_image cfg_p;
  reg_image cfg_pwi;
  reg_image cfg_pwl

(* Secret *)
let secret_id = ref 0
let newsecretid () =
  incr secret_id;
  string_of_int (!secret_id)

let () =
  Tex.register_env cfg_f5 "secret"
  (fun s ->
    let defaulttext = "Secret (click to show/hide)" in
    let labelcontent = Tex.read_opt cfg_pwl s [pcdata defaulttext] in
    (labelcontent, newsecretid ())
  )
  (fun _ -> cfg_f5)
  (fun (labelcontent, sid) content -> [
    label ~a:[a_label_for sid] labelcontent;
    input ~a:[a_id sid; a_class ["secretcb"]; a_input_type `Checkbox] ();
    div ~a:[a_class ["secretdiv"]] content
  ])


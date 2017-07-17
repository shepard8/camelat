(*
 * Camelat - Parsing LaTeX-like text in OCaml
 * Copyright (C) year  name of author
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

open Eliom_content.Html.D

type 'a cfg = 'a Cfg.cfg

let r_nl = Str.regexp "$"
let smileys = [
  ":)", "smile";
  ":-)", "smile";
  ";)", "wink";
  ";-)", "wink";
  ":(", "sad";
  ":-(", "sad"
]
let r_nl_smileys = Str.regexp (String.concat "\\|" ("$" :: List.map (fun (s, _) -> Str.quote s) smileys))
let text s =
  let parts = Str.full_split r_nl_smileys s in
  List.map (function
    | Str.Text t -> pcdata t
    | Str.Delim "" | Str.Delim "\n" -> br ()
    | Str.Delim smiley -> 
        let path = ["smileys"; List.assoc smiley smileys] in
        let src = make_uri (Eliom_service.static_dir ()) path in
        img ~a:[a_title "smiley"] ~src ~alt:smiley ()
  ) parts

type pwi = Html_types.phrasing_without_interactive
type pwl = Html_types.phrasing_without_label
type p = Html_types.phrasing
type f5 = Html_types.flow5

let cfg_pwi : pwi elt list_wrap cfg = Cfg.init text List.concat
let cfg_pwl : pwl elt list_wrap cfg = Cfg.init text List.concat
let cfg_p : p elt list_wrap cfg = Cfg.init text List.concat
let cfg_f5 : f5 elt list_wrap cfg = Cfg.init text List.concat

let reg_wrap cfg name f sub =
  Cfg.register_cmd cfg name (fun s -> [f (Cfg.read_arg sub s)])

(* Bold, italic, underline, strike *)
let reg_style cfg name style sub =
  reg_wrap cfg name (fun a -> span ~a:[a_style style] a) sub

let () =
  let styles = [
    ("textbf", "font-weight: bold");
    ("textit", "font-style: italic");
    ("underline", "text-decoration: underline");
    ("sout", "text-decoration: line-through");
    ("textrm", "font-family: serif");
    ("textsf", "font-family: sans-serif");
    ("texttt", "font-family: monospace");
    ("textsc", "font-variant: small-caps");
    ("tiny", "font-size: 6pt");
    ("scriptsize", "font-size: 8pt");
    ("footnotesize", "font-size: 9pt");
    ("small", "font-size: 10pt");
    ("normalsize", "font-size: 10.95pt");
    ("large", "font-size: 12pt");
    ("Large", "font-size: 14.4pt");
    ("LARGE", "font-size: 17.28pt");
    ("huge", "font-size: 20.74pt");
    ("Huge", "font-size: 24.88pt");
   ] in
  List.iter (fun (n, s) -> reg_style cfg_pwi n s cfg_pwi) styles;
  List.iter (fun (n, s) -> reg_style cfg_pwl n s cfg_p) styles;
  List.iter (fun (n, s) -> reg_style cfg_p n s cfg_p) styles;
  List.iter (fun (n, s) -> reg_style cfg_f5 n s cfg_p) styles

(* Color *)
let reg_color cfg sub =
  Cfg.register_cmd cfg "textcolor" (fun s ->
    let color = Cfg.read_arg Cfg.cfg_text s in
    let arg = Cfg.read_arg sub s in
    [ span ~a:[a_style ("color: " ^ color ^ ";")] arg ]
  )

let () =
  reg_color cfg_pwi cfg_pwi;
  reg_color cfg_pwl cfg_p;
  reg_color cfg_p cfg_p;
  reg_color cfg_f5 cfg_p

(* Sectionning *)
let () =
  reg_wrap cfg_f5 "section" h3 cfg_p;
  reg_wrap cfg_f5 "subsection" h4 cfg_p;
  reg_wrap cfg_f5 "subsubsection" h5 cfg_p

(* Link *)
let reg_link cfg =
  Cfg.register_cmd cfg "link" (fun s ->
    let url = Cfg.read_opt Cfg.cfg_text s "" in
    if url = "" then
      let arg = Cfg.read_opt Cfg.cfg_text s "" in
      [ Raw.a ~a:[a_href (uri_of_string (fun () -> arg))] [pcdata arg] ]
    else
      let arg = Cfg.read_arg cfg_pwi s in
      [ Raw.a ~a:[a_href (uri_of_string (fun () -> url))] arg ]
  )

let () =
  reg_link cfg_pwl;
  reg_link cfg_p

let () =
  Cfg.register_cmd cfg_f5 "link" (fun s ->
    let url = Cfg.read_opt Cfg.cfg_text s "" in
    if url = "" then
      let arg = Cfg.read_opt Cfg.cfg_text s "" in
      [ Raw.a ~a:[a_href (uri_of_string (fun () -> arg))] [pcdata arg] ]
    else
      let arg = (Cfg.read_arg cfg_pwi s :
        pwi elt list_wrap :>
        Html_types.flow5_without_interactive elt list_wrap
      ) in
      [ Raw.a ~a:[a_href (uri_of_string (fun () -> url))] arg ]
  )

(* List *)
let cfg_item = Cfg.init (fun _ -> []) List.concat
let () =
  Cfg.register_cmd cfg_item "item" (fun s -> [li (Cfg.read_item cfg_f5 "item" s)])

let () =
  Cfg.register_env cfg_f5 "itemize"
  (fun _ -> ())
  (fun () -> cfg_item)
  (fun () content -> [ul content])


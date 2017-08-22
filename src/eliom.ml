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

(* Convention : reg_* functions apply to one configuration, while register_*
 * functions apply to as much configurations as possible. *)

open Eliom_content.Html.D

type f5 = Html_types.flow5 elt
type f5wi = Html_types.flow5_without_interactive elt
type p = Html_types.phrasing elt
type pwi = Html_types.phrasing_without_interactive elt

type t = {
  f5 : f5 list Cfg.cfg;
  f5wi : f5wi list Cfg.cfg;
  p : p list Cfg.cfg;
  pwi : pwi list Cfg.cfg;
}

let text smileys smileys_path endline_br s =
  let split_on = "$" :: List.map (fun (s, _) -> Str.quote s) smileys in
  let r = Str.regexp (String.concat "\\|" split_on) in
  let parts = Str.full_split r s in
  List.map (function
    | Str.Text t -> pcdata t
    | Str.Delim "" -> pcdata ""
    | Str.Delim "\n" -> if endline_br then br () else pcdata ""
    | Str.Delim smiley ->
        let path = smileys_path @ [List.assoc smiley smileys] in
        let src = make_uri (Eliom_service.static_dir ()) path in
        img ~src ~alt:smiley ()
  ) parts

let eliominit ?(smileys=[]) ?(smileys_path=["smileys"]) ?(endline_br=true) () =
  let text = text smileys smileys_path endline_br in
  {
    f5 = Cfg.init text List.concat;
    f5wi = Cfg.init text List.concat;
    p = Cfg.init text List.concat;
    pwi = Cfg.init text List.concat;
  }

let reg_wrap cfg name f sub =
  Cfg.register_cmd cfg name (fun s -> [f (Cfg.read_arg sub s)])

let reg_style cfg name f_style sub =
  Cfg.register_cmd cfg name (fun s ->
    let style = f_style s in
    [span ~a:[a_style style] (Cfg.read_arg sub s)]
  )

let register_style t name f_style =
  reg_style t.f5 name f_style t.p;
  reg_style t.f5wi name f_style t.pwi;
  reg_style t.p name f_style t.p;
  reg_style t.pwi name f_style t.pwi

let reg_escape cfg name f =
  Cfg.register_cmd cfg name (fun s ->
    let c = f s in
    [ pcdata c ]
  )

let register_escape t name f =
  reg_escape t.f5 name f;
  reg_escape t.f5wi name f;
  reg_escape t.p name f;
  reg_escape t.pwi name f


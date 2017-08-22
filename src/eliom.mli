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

(**
 * Building a Camelat configuration generating TyXML is not trivial as several
 * types have to be handled. As stated in the documentation, the HTML elements
 * are mainly subdivided in three categories : interactive (e.g., some "a"
 * element), phrasing (anything that can be placed inside some "span" element)
 * and flow5 (anything that can be placed directly inside the "body" element).
 *
 * This module defines a type [t] that will handle four distinct configurations:
 * - f5 stands for flow5, this configuration is typically the entry point. When
 * parsing some LaTeX-like code with this configuration, the result can for
 * example be used inside a "div" element.
 * - p stands for phrasing, this configuration parses LaTeX-like code into
 * elements that can be used inside a "span" element.
 * - pwi stands for phrasing_without_interaction, resulting elements can be
 * used inside some "a" element. Note that in HTML5, the "a" element can not
 * contain other "a" elements.
 * - pwl stands for phrasing_without_label, resulting elements can be used
 * inside a label. This is essentially useful for some "secret" text that can
 * be shown or hidden using a checkbox.
 *
 * In addition to this type, some facilities allow you to define new LaTeX
 * commands that are automatically added to the relevant configurations.
 *)

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

val eliominit : ?smileys:(string * string) list -> ?smileys_path:string list -> ?endline_br:bool -> unit -> t

val reg_wrap : 'a list Cfg.cfg -> Cfg.csname -> ('b -> 'a) -> 'b Cfg.cfg -> unit

val register_style : t -> Cfg.csname -> string -> unit

type 'a param = Arg of 'a Cfg.cfg | Opt of ('a Cfg.cfg * 'a)

val parg : string param
val popt : string -> string param

val register_style_param : t -> Cfg.csname -> 'a param -> ('a -> string) -> unit

val register_a : t -> Cfg.csname -> 'a param -> ('a -> p) -> unit

val register_a_param : t -> Cfg.csname -> 'a param -> 'b param -> ('a -> 'b -> p) -> unit


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
 * - pwi stands for phrasing_without_interactive, resulting elements can be
 * used inside some "a" element. Note that in HTML5, the "a" element can not
 * contain other "a" elements.
 * - f5wi stands for flow5_without_interactive, resulting elements can be used
 * inside some "a" element.
 *
 * In addition to this type, some facilities allow you to define new LaTeX
 * commands that are automatically added to the relevant configurations.
 *
 * In this module, functions prefixed by [reg_] apply to configurations while
 * functions prefixed by [register_] apply to as much configurations in the [t]
 * type as possible.
 *)

open Camelat
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

val eliominit : ?smileys:(string * string) list ->
  ?smileys_path:string list -> ?endline_br:bool -> unit -> t
(** [eliominit ()] initializes the type [t], creating four configurations (in
 * the sense of {Cfg.t}), which handle distinct regions in a LaTeX document. It
 * is possible to register new LaTeX commands/environments to any of these
 * configurations. This is typically done for the f5 configuration, whose
 * commands/environments are the only able to use f5 elements.
 *
 * ?smileys allows to register strings that will be substituted by an image.
 * Default is [[]].
 *
 * ?smileys_path allows to register the directory in which smiley images can be
 * found. This is a list of directory names like those used by eliom. Default
 * is [["static"; "smileys"]].
 *
 * ?endline_br allows to specify whether endlines have to be replaced by <br />
 * tags or not. Default is [true].
 * *)

val reg_wrap : 'a list Cfg.cfg -> Cfg.csname -> ('b -> 'a) -> 'b Cfg.cfg -> unit
(** [reg_wrap cfg csname f sub] registers a command [csname] in configuration
 * [cfg] that will apply [f] to its argument, which will be captured using
 * configuration [sub]. *)

val register_style : t -> Cfg.csname -> (Cfg.source -> string) -> unit
(** [register_style t csname f_style] registers a command [csname] in all four
 * of [t] configurations. The [f_style] function can consume elements from the
 * source to produce a style string, which will be applied to a <span> tag
 * around the next command argument, itself captured by the largest
 * configuration possible:
 * - In t.f5 and t.p, the last argument will be captured using t.p;
 * - In t.f5wi and t.pwi, the last argument will be captured using t.pwi.
 *
 * Example with static style:
 *    
 *    register_style t "textbf" (fun _ -> "font-weight: bold")
 *
 * Example with dynamic style:
 *
 *    register_style t "size" (fun s ->
 *      let size = Cfg.read_arg Cfg.cfg_int s in
 *      sprintf "font-size: %d%%" size
 *    )
 *
 * *)

val register_leaf : t -> Cfg.csname -> (Cfg.source -> pwi list) -> unit
(** [register_leaf t csname f] allows you to register something in all four
 * configurations at once. The downside being that the generated element
 * therefore needs to be of the most restrictive type; {pwi}. It is still
 * useful to register commands that have no children such as images.  *)

val register_escape : t -> Cfg.csname -> (Cfg.source -> string) -> unit
(** [register_escape t csname f_escape] registers a command [csname] in all
 * four of [t] configurations. The command will produce text in a pcdata
 * container, possibly consuming a part of the source. *)


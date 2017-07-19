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

type f5 = Html_types.flow5 elt
type p = Html_types.phrasing elt
type pwi = Html_types.phrasing_without_interactive elt
type pwl = Html_types.phrasing_without_label elt

type t = {
  f5 : f5 list Cfg.cfg;
  p : p list Cfg.cfg;
  pwi : pwi list Cfg.cfg;
  pwl : pwl list Cfg.cfg;
}

val eliominit : ?smileys:(string * string) list -> ?smileys_path:string list -> ?endline_br:bool -> unit -> t

val reg_wrap : 'a list Cfg.cfg -> Cfg.csname -> ('b -> 'a) -> 'b Cfg.cfg -> unit

val register_style : t -> Cfg.csname -> string -> unit

type 'a param = Arg of 'a Cfg.cfg | Opt of ('a Cfg.cfg * 'a)

val parg : string param
val popt : string -> string param

val register_style_param : t -> Cfg.csname -> 'a param -> ('a -> string) -> unit

val register_a : t -> Cfg.csname -> 'a param -> ('a -> [< `A of [< Html_types.phrasing_without_interactive] & [< Html_types.phrasing_without_interactive] & [< Html_types.flow5_without_interactive] ] elt) -> unit


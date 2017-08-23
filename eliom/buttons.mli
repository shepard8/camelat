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
 * Websites using Camelat usually allow their user to type in LaTeX-like code
 * somewhere. It is then convenient to add buttons next to the textarea zone
 * that allow users to easily format their text. Although this module does not
 * allow some WYSIWYG editing, it still avoid the users the need to remember
 * every command available.
 *
 * Note that for these commands to work, the javascript code associated needs
 * to be loaded. Using Ocsigen, this is usually done in two steps. First add
 * the file js/camelat.js of this repository to the static/js directory of the
 * website, then load it within the head section.
 *)

open Camelat
open Eliom_content.Html.D
type id = string
(** The optional ~id argument in each of this module's functions allows you to
 * put more than one textarea with buttons associated on the same page. Unless
 * you have several textarea which you want to support camelat buttons, you
 * should not worry about ids. *)

val textarea : ?id:id ->
  ?a:Html_types.textarea_attrib attrib list ->
  name:[< string Eliom_parameter.setoneradio] Eliom_parameter.param_name ->
  ?value:string -> unit -> [> Html_types.textarea] elt
(** [textarea ?id] creates a textarea the same way Form.textarea does. It
 * allows not having to bother with the id of the textarea should there be only
 * one on the page. *)

val cmd : ?toggle : bool -> Cfg.csname -> ?id:id -> (
  [< Html_types.button_attrib],
  [< Html_types.button_content_fun],
  [> Html_types.button]
) star
(** [cmd ?toggle csname ?id] creates a button for the command [csname].
 *
 * The optional parameter ~toggle ([true] by default) allows to chose what
 * happens when the selection starts with "\\[csname]{" and ends with "}". When
 * ~toggle=true, the outermost command will be removed; otherwise another
 * [csname] command will be added around the selection. One use case when you'd
 * want to set toggle to false is for smileys buttons, for example. *)

val env : Cfg.csname -> ?id:id -> (
  [< Html_types.button_attrib],
  [< Html_types.button_content_fun],
  [> Html_types.button]
) star
(** [env csname ?id] creates a button for the environment [csname]. The
 * selection will be automatically padded with tabulations. *)

val lst : Cfg.csname -> Cfg.csname -> ?id:id -> (
  [< Html_types.button_attrib],
  [< Html_types.button_content_fun],
  [> Html_types.button]
) star
(** [lst envname itmname ?id] creates a button for a list environment. Each
 * line will be prepended with the [itmname] command. *)



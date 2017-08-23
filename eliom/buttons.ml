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

open Camelat
open Eliom_content.Html.D
open Printf

let add js =
  let attr = to_attrib (Xml.string_attrib "onclick" js) in
  function
    | None -> [attr]
    | Some l -> attr :: l

type id = string

let default_id : id = "camelat-ta"

let textarea ?(id=default_id) ?a ~name ?value () =
  let a = match a with
  | None -> [a_id id]
  | Some l -> a_id id :: l
  in Form.textarea ?value ~name ~a ()

let cmd ?(toggle=true) csname ?(id=default_id) ?a content =
  let jsfn = if toggle then "switchcmd" else "surround" in
  let js = sprintf "%s('%s', '\\\\%s{', '}'); return false;" jsfn id csname in
  button ~a:(add js a) content

let env csname ?(id=default_id) ?a content =
  let js = sprintf "switchenv('%s', '\\\\begin{%s}\\n', '\\n\\\\end{%s}'); return false;" id csname csname in
  button ~a:(add js a) content

let lst csname csitem ?(id=default_id) ?a content =
  let js = sprintf "switchgen('%s', '\\\\begin{%s}\\n', '\\n\\\\end{%s}', '\\\\%s '); return false;" id csname csname csitem in
  button ~a:(add js a) content

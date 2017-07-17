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

let service_result =
  Eliom_registration.Html.create
  ~path:(Eliom_service.Path ["result"])
  ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.string "text"))
  (fun () text ->
    Lwt.return (
      html (
        head (title (pcdata "TexML Web Example")) []
      ) (
        body (
          match Cfg.parse Eliom.cfg_f5 text with
          | Ok v -> v
          | Error (v, errors) -> pcdata "Errors : " ::
            ul (List.map (fun (i, e) -> li [pcdata (Printf.sprintf "%d : %s" i
            (match e with
            | Cfg.Unknown_command csname -> "Unknown command " ^ csname
            | Cfg.Unknown_environment csname -> "Unknown environment " ^ csname
            | Cfg.Misplaced_end csname -> "Misplaced end " ^ csname
            | Cfg.Unexpected_eof -> "Unexpected EOF"
            ))]) errors) :: hr () :: v
        )
      )))

let parse text = match Cfg.parse Eliom.cfg_pwi text with
| Ok v -> v
| Error (v, _) -> v

let service_home =
  Eliom_registration.Html.create
  ~path:(Eliom_service.Path ["home"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  (fun () () ->
    Lwt.return (
      html (
        head (title (pcdata "TexML Web Example")) [
          js_script ~uri:(make_uri (Eliom_service.static_dir ()) ["js"; "camelat.js"]) ();
        ]
      ) (
        body [
          Form.post_form service_result (fun text -> [
            Buttons.cmd "textbf" (parse "\\textbf{B}");
            Buttons.cmd "textit" (parse "\\textit{I}");
            Buttons.cmd "underline" (parse "\\underline{U}");
            Buttons.cmd "sout" (parse "\\sout{S}");
            Buttons.cmd ~toggle:false "notacommand" (parse "NC");
            Buttons.env "notanenvironment" (parse "NE");
            Buttons.lst "itemize" "item" [pcdata "&bullet;"];
            br ();
            Buttons.textarea ~a:[a_rows 20; a_cols 80] ~name:text ();
            br ();
            Form.input ~input_type:`Submit ~value:"View Result" Form.string
          ]) ()
        ])))


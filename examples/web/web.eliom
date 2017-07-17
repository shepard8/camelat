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


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

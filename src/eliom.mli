open Eliom_content.Html.D

type coint = Html_types.phrasing_without_interactive
type phrasing = Html_types.phrasing
type full = Html_types.flow5

val cfg_full : full elt list Tex.cfg
val cfg_phrasing : phrasing elt list Tex.cfg
val cfg_coint : coint elt list Tex.cfg


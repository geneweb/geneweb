open Geneweb.Config
open Geneweb.Util
open Geneweb.DescendDisplay

module DescendDisplay = Geneweb.DescendDisplay


let print conf base p =
  let templ =
    match p_getenv conf.env "t" with
      Some ("F" | "L" | "M") -> "deslist"
    | Some "D" -> "deslist_hr"
    | Some ("H" | "I" | "A") -> "destable"
    | Some "V" -> "destree"
    | Some _ -> ""
    | _ -> "desmenu"
  in
  if templ <> "" then !V7_interp.templ templ conf base p
  else
    match p_getenv conf.env "t", p_getint conf.env "v" with
      Some "B", Some v -> DescendDisplay.print_aboville conf base v p
    | Some "S", Some v -> DescendDisplay.display_descendants_level conf base v p
    | Some "K", Some v -> DescendDisplay.display_descendant_with_table conf base v p
    | Some "N", Some v -> DescendDisplay.display_descendants_with_numbers conf base v p
    | Some "G", Some v -> DescendDisplay.display_descendant_index conf base v p
    | Some "C", Some v -> DescendDisplay.display_spouse_index conf base v p
    | Some "T", Some v -> DescendDisplay.print_tree conf base v p
    | _ -> desmenu_print conf base p



let is_text = function
  | `Text _ -> true
  | `Comment _ | `Doctype _ | `End_element | `PI _ | `Start_element _ | `Xml _
    ->
      false

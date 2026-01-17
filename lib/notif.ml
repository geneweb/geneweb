type level = Info | Success | Warning | Err
type mode = Auto | Dismissible
type t = { level : level; title : string; message : string; mode : mode }

let queue : t list ref = ref []
let clear () = queue := []

let add ?(mode = Auto) ?(title = "") level message =
  let mode = match level with Err -> Dismissible | _ -> mode in
  queue := { level; title; message; mode } :: !queue

let info ?(mode = Auto) ?(title = "") msg = add ~mode ~title Info msg
let success ?(mode = Auto) ?(title = "") msg = add ~mode ~title Success msg
let warning ?(mode = Auto) ?(title = "") msg = add ~mode ~title Warning msg
let error ?(title = "") msg = add ~title Err msg
let has_pending () = !queue <> []

let level_to_class = function
  | Info -> "info"
  | Success -> "success"
  | Warning -> "warning"
  | Err -> "danger"

let level_to_icon = function
  | Info -> "fa-circle-info"
  | Success -> "fa-circle-check"
  | Warning -> "fa-triangle-exclamation"
  | Err -> "fa-circle-xmark"

let mode_to_autohide = function Auto -> true | Dismissible -> false

let render_json () =
  let notifs = List.rev !queue in
  let json =
    `List
      (List.map
         (fun n ->
           `Assoc
             [
               ("level", `String (level_to_class n.level));
               ("icon", `String (level_to_icon n.level));
               ("title", `String n.title);
               ("message", `String n.message);
               ("autohide", `Bool (mode_to_autohide n.mode));
             ])
         notifs)
  in
  clear ();
  Yojson.Basic.to_string json

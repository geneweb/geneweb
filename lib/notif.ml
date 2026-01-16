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

let mode_to_autohide = function Auto -> true | Dismissible -> false

let inject_pending conf =
  if not (has_pending ()) then conf
  else
    let existing =
      match List.assoc_opt "notif" conf.Config.env with
      | Some v -> (
          try
            match Yojson.Basic.from_string (v :> string) with
            | `List l -> l
            | _ -> []
          with _ -> [])
      | None -> []
    in
    let new_items =
      List.map
        (fun n ->
          `Assoc
            [
              ("level", `String (level_to_class n.level));
              ("title", `String n.title);
              ("message", `String n.message);
              ("autohide", `Bool (mode_to_autohide n.mode));
            ])
        (List.rev !queue)
    in
    clear ();
    let json = Yojson.Basic.to_string (`List (existing @ new_items)) in
    {
      conf with
      env = ("notif", Adef.encoded json) :: List.remove_assoc "notif" conf.env;
    }

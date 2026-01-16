(** Server-to-client notification system using Bootstrap toasts. *)

type level =
  | Info
  | Success
  | Warning
  | Err  (** Notification severity level. Affects toast styling. *)

type mode =
  | Auto
  | Dismissible
      (** Auto: toast disappears after 6s. Dismissible: requires user click. *)

val clear : unit -> unit
(** Clear all pending notifications. *)

val info : ?mode:mode -> ?title:string -> string -> unit
(** Queue info notification (blue). Default mode: Auto. *)

val success : ?mode:mode -> ?title:string -> string -> unit
(** Queue success notification (green). Default mode: Auto. *)

val warning : ?mode:mode -> ?title:string -> string -> unit
(** Queue warning notification (yellow). Default mode: Auto. *)

val error : ?title:string -> string -> unit
(** Queue error notification (red). Always Dismissible. If title is empty,
    default label from lexicon is used. *)

val has_pending : unit -> bool
(** Check if notifications are pending. *)

val inject_pending : Config.config -> Config.config
(** Inject pending notifications into conf.env as notif. Merges with any
    existing notifications. Clears queue. Returns conf unchanged if empty. *)

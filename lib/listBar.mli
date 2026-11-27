(** Long lists sent a slice at a time: [n] rows from row [from], the rows chosen
    by their beginning [bw], and the bar of the page that changes them. Used by
    the pages of spellings to group ({!PlaceTermsDisplay}) and by m=PS
    ({!PlaceDisplay}). *)

val page_sizes : int list
(** The sizes offered in the bar: 25, 50, 100, 200, 500, 1000. *)

val page_size : ?gwf_key:string -> Config.config -> int
(** Number of rows of a slice: [n] chosen on the page, else the value of
    [gwf_key] in the [.gwf] ([terms_page_size] by default), else 100; [max_int]
    with [n=all]. *)

val all_rows : Config.config -> bool
(** Whether the page asks for every row on one page: [n=all]. *)

val slice : ?gwf_key:string -> Config.config -> int -> int * int
(** [slice conf total] is [(from, upto)], the rows shown, counted from 1: [from]
    asked by the page (1 if absent or out of range) and {!page_size} rows;
    [(1, total)] with [n=all]. *)

val n_param : ?gwf_key:string -> Config.config -> string
(** The value of [n] to keep in the links of the page: ["all"] or {!page_size}.
*)

val take : int * int -> 'a list -> 'a list
(** [take (from, upto) l] The rows [from] to [upto] of [l], counted from 1. *)

val begins_with : Config.config -> string option
(** The beginning [bw] asked by the page, if not empty. *)

val fold_case : string -> string
(** [s] lowercased and unaccented, its spaces and punctuation kept (unlike
    {!Name.lower}): ["33 "] and ["33,"] are different beginnings. *)

val begins_ok : Config.config -> string -> bool
(** [begins_ok conf s] Whether [s] starts with {!begins_with}, case and accents
    ignored; always [true] without [bw]. *)

val print_list_bar :
  ?gwf_key:string ->
  ?begins:bool ->
  ?offer_all:bool ->
  Config.config ->
  href:string ->
  hidden:(unit -> unit) ->
  (int * (int * int)) option ->
  unit
(** The bar of a long list: the field [bw], the size of the slices, the previous
    and next slices. [href] is the page without [bw], [n] and [from]; [hidden]
    emits its fields. Changing [bw] or [n] goes back to the first row. [counts]
    is the number of rows and the slice shown; without it (nothing computed
    yet), a button shows all the rows ([all=on]). Without [begins] ([true] by
    default), the field [bw] is left out. With [offer_all] ([false] by default),
    the size of the slices may be "all" and a button shows every row ([n=all])
    when they do not all fit on the page. *)

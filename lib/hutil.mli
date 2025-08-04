open Config

type 'a value =
  | Vint of int
  | Vother of 'a
  | Vnone  (** Polymorphic value type for template environment *)

module BufferPool : sig
  val with_buffer : (Buffer.t -> 'a) -> 'a
  (** [with_buffer f] provides a buffer from the pool to [f], automatically
      returning it to the pool after use. Uses small buffer pool (2KB) for
      efficiency. *)
end

module TemplateCache : sig
  val get_or_load : Config.config -> string -> string option
  (** [get_or_load conf name] retrieves template [name] from cache or loads it
      if not present. Returns [None] if template doesn't exist. Uses LRU
      eviction when cache exceeds 10MB. *)
end

module HtmlBuffer : sig
  val create_buffered_conf : ?initial_size:int -> Config.config -> Config.config
  (** [create_buffered_conf ~initial_size conf] returns a config with buffered
      output. All writes accumulate in a buffer until [flush] is called.
      @param initial_size Initial buffer size in bytes (default: 65536) *)

  val wrap : Config.config -> (Config.config -> 'a) -> 'a
  (** [wrap ~initial_size conf f] executes [f] with a buffered config,
      automatically flushing at the end or on exception. Uses page buffer pool
      for efficiency. *)

  val wrap_measured : Config.config -> (Config.config -> 'a) -> 'a
  (** [wrap_measured conf f] like [wrap] but collects performance metrics. If
      ["debug_buffer"] is in [conf.env], prints statistics to stderr:
      - Number of writes
      - Total bytes written
      - Time elapsed
      - Memory allocated *)
end

val get_env : string -> 'a value Templ.Env.t -> 'a value
(** [get_env key env] retrieves value from template environment. Returns [Vnone]
    if key doesn't exist. *)

val get_vother : 'a value -> 'a option
(** [get_vother v] extracts value if [v = Vother x], else [None]. *)

val set_vother : 'a -> 'a value
(** [set_vother x] wraps value in [Vother] constructor. *)

val header_without_http_nor_home : config -> (bool -> unit) -> unit
(** Base header without HTTP headers or home template. The boolean parameter
    determines if title is printed in <title> tag. *)

val header_with_title :
  ?error:bool -> ?fluid:bool -> config -> (bool -> unit) -> unit
(** Complete header with title.
    @param error Display title in red if [true] (default: false)
    @param fluid Use container-fluid if [true] (default: false)
    @param conf Base configuration
    @param title Title function (bool indicates <title> vs <h1>) *)

val header_fluid : config -> (bool -> unit) -> unit
(** Shortcut for [header_with_title ~fluid:true]. *)

val header_with_conf_title : config -> (bool -> unit) -> unit
(** Header that gets title from [conf.env "p_title"]. *)

val header_without_title : config -> unit
(** Complete header without <h1> title element. *)

val header_without_home : config -> (bool -> unit) -> unit
(** Like [header_with_title] but without home.txt inclusion. *)

val header : ?error:bool -> ?fluid:bool -> config -> (bool -> unit) -> unit
(** Main header interface. Alias for [header_with_title].
    @param error Display title in red if [true]
    @param fluid Use container-fluid if [true] *)

val rheader : config -> (bool -> unit) -> unit
(** Alias for [header ~error:true]. Shows error title (red). *)

val trailer : config -> unit
(** Standard HTML page footer. Outputs in order:
    - "trl" template (user trailer)
    - "copyr" template (copyright)
    - Container closure
    - "js" template (JavaScript)
    - Debug timing if enabled
    - </body></html> tags *)

val trailer_with_extra_js : config -> string -> unit
(** [trailer_with_extra_js conf sources] outputs trailer with additional
    JavaScript injection before main js.txt.

    @param sources
      JavaScript sources to inject:
      - Single template: ["template_name"]
      - Multiple templates: ["templ1|templ2|templ3"]
      - Inline JavaScript: ["<script>...</script>"]

    Templates are cached for performance. *)

val link_to_referer : config -> Adef.safe_string
(** Returns HTML link to previous page (referer). Returns empty string if no
    referer available. *)

val incorrect_request : ?comment:string -> config -> unit
(** Sends HTTP 400 Bad Request response.
    @param comment Optional error message *)

val error_cannot_access : config -> string -> unit
(** Sends HTTP 404 Not Found when template cannot be accessed.
    @param fname Name of inaccessible file *)

val include_home_template : config -> unit
(** Includes home.txt template if available. Silent if file doesn't exist. *)

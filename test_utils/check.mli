(** Helper module for concise Alcotest assertions and test definitions. *)

val check :
  ?pos:string * int * int * int ->
  'a Alcotest.testable ->
  ?msg:string ->
  'a ->
  'a ->
  unit
(** [check ?pos testable ?msg expected actual] asserts that [actual] matches
    [expected] using [testable]. If [pos] is provided, uses [Alcotest.check']
    with source code location; otherwise uses [Alcotest.check] with [msg]
    (defaulting to empty string). *)

val string :
  ?pos:string * int * int * int -> ?msg:string -> string -> string -> unit
(** [string ?pos ?msg expected actual] asserts equality between two strings. *)

val int : ?pos:string * int * int * int -> ?msg:string -> int -> int -> unit
(** [int ?pos ?msg expected actual] asserts equality between two integers. *)

val bool : ?pos:string * int * int * int -> ?msg:string -> bool -> bool -> unit
(** [bool ?pos ?msg expected actual] asserts equality between two booleans. *)

val list :
  'a Alcotest.testable ->
  ?pos:string * int * int * int ->
  ?msg:string ->
  'a list ->
  'a list ->
  unit
(** [list testable ?pos ?msg expected actual] asserts equality between two lists. *)

val option :
  'a Alcotest.testable ->
  ?pos:string * int * int * int ->
  ?msg:string ->
  'a option ->
  'a option ->
  unit
(** [option testable ?pos ?msg expected actual] asserts equality between two optional values. *)

val pair :
  'a Alcotest.testable ->
  'b Alcotest.testable ->
  ?pos:string * int * int * int ->
  ?msg:string ->
  'a * 'b ->
  'a * 'b ->
  unit
(** [pair t1 t2 ?pos ?msg expected actual] asserts equality between pairs. *)

val string_pair :
  ?pos:string * int * int * int ->
  ?msg:string ->
  string * string ->
  string * string ->
  unit
(** [string_pair ?pos ?msg expected actual] asserts equality between string pairs. *)

val check_raises :
  ?pos:string * int * int * int -> string -> exn -> (unit -> unit) -> unit
(** [check_raises ?pos msg exn f] asserts that evaluating [f ()] raises [exn]. *)

val fail : ?pos:string * int * int * int -> string -> 'a
(** [fail ?pos msg] unconditionally fails a test with [msg]. *)

val case : string -> (unit -> unit) -> unit Alcotest.test_case
(** [case name f] creates a quick Alcotest test case. *)

val suite :
  string ->
  unit Alcotest.test_case list ->
  string * unit Alcotest.test_case list
(** [suite name cases] pairs a suite name with its list of test cases. *)

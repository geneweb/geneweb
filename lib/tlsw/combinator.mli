type +'a t
(** Type of parsers that produce values of type ['a]. *)

type ('a, 'st) res = Ok of 'a * 'st | Fail of (unit -> string) * 'st

(** {2 Monadic operators} *)

val run : 'a t -> Input.t -> ('a, Input.t) res
(** [run t st] executes the parser [t] starting from the initial state [st]. *)

val ret : 'a -> 'a t
(** [ret a] creates a parser that always succeeds and returns the value [a]
    without consuming any input. *)

val fail : ('b, Format.formatter, unit, 'a t) format4 -> 'b
(** [fail] creates a parser that always fails with consuming any input. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind t f] executes the parser [t]. If [t] succeeds, its result is passed to
    the function [f]. *)

(* {2 Parsers for basic types} *)

val char : char -> char t
(** [char c] creates a parser that accepts exactly the character [c]. *)

val string : string -> string t
(** [string s] creates a parser that accepts exactly the string [s]. *)

val digit : int t
(** [digit] is a parser accepting a single decimal digit. *)

val int : int t
(** [int] is parser accepting decimal numbers. *)

(** {2 Basic combinators} *)

val choice : 'a t list -> 'a t
(** [choice l] attemps to run the parsers in the list [l] in order. It succeeds
    with the result of the first parser that succeeds. *)

val many : 'a t -> 'a list t
(** [many t] repeatedly executes [t] as long as it succeeds, accumulating all
    results into a list. *)

val many1 : 'a t -> 'a list t
(** [many1 t] does the same as [many] but [t] must succeed at least once for
    [many1 t] to succeed. *)

val option : 'a t -> 'a option t
(** [option t] attempts to execute parser [t]. If [t] succeeds, its result is
    wrapped in [Some]. If [t] fails, [option t] succeeds and returns [None]. *)

val case : (_ t * 'a t) list -> 'a t
(** [case l] runs in order the first element of the pairs until one succeed.
    Then it runs the second element. *)

val until : _ t -> string t
(** [until t] accumulates the input until the parser [t] succeeds. *)

val skip : _ t -> unit t
(** [skip t] executes [t], consuming input as [t] woud, but discards its result.
*)

val count : 'a t -> int t
(** [count t] executes [t] and returns the number of characters consumed by [t].
    This combinator is more efficient than [List.length (many t)]. *)

val located : 'a t -> ('a * Loc.t) t
(** [with_loc t] executes parser [t]. If [t] succeeds, it returns both the
    result of [t] and the span of input consumed by [t]. *)

(* {2 Syntax extension} *)

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( <|> ) : 'a t -> 'a t -> 'a t
val ( *> ) : _ t -> 'a t -> 'a t
val ( <* ) : 'a t -> _ t -> 'a t

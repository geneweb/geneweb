type +'a t
(** Type of parsers accepting value of type ['a]. *)

val run : 'a t -> Input.t -> ('a * Input.t) option
(** [run t st] runs the parser [t] on the initial state [st]. *)

val ret : 'a -> 'a t
(** [ret a] produces the parser that always succeeds and returns [a]. *)

val fail : 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind t f] invokes [f] with the result of the parser [t], if it succeeds. *)

val choice : 'a t list -> 'a t
(** [choice l] runs the parsers [l] in ascending order. If it fails, run the
    parser [t2]. *)

val case : (_ t * 'a t) list -> 'a t

val until : _ t -> string t
(** [until t] accumulates the input until the parser [t] succeeds. *)

val skip : _ t -> unit t
val many : 'a t -> 'a list t
val many1 : 'a t -> 'a list t
val count : 'a t -> int t
val option : 'a t -> 'a option t
val with_loc : 'a t -> ('a * Loc.t) t

val char : char -> char t
(** [char c] produces the parser that accepts exactly [c]. *)

val string : string -> string t
(** [string s] produces the parser that accepts exactly [s]. *)

val digit : int t
val int : int t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( <|> ) : 'a t -> 'a t -> 'a t
val ( *> ) : _ t -> 'a t -> 'a t
val ( <* ) : 'a t -> _ t -> 'a t

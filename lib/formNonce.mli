(** Nonces of the update forms.

    Each update form carries a nonce drawn when it is displayed. A request which
    writes into the base records its nonce, with a fingerprint of the parameters
    posted, in [base.nonces] next to the lock file. The same form posted again
    with the same parameters, by a double click, by the browser after an error
    of the proxy, or from a page offering to post it again, is then answered
    "already saved" instead of being applied twice: a family or a person created
    twice. The same form posted with other parameters (a form left open,
    modified and saved again) is not refused: the digest of the form handles it.
*)

val fresh : unit -> string
(** A new nonce. *)

val fingerprint : (string * Adef.encoded_string) list -> string
(** [fingerprint env] is a fingerprint of the parameters [env] of a request, the
    nonce excepted. *)

val saved : string -> nonce:string -> fingerprint:string -> bool
(** [saved bfile ~nonce ~fingerprint] tells whether a request of [nonce] and
    [fingerprint] already wrote into the base [bfile]. To be called while
    holding the lock of the base. *)

val record : string -> nonce:string -> fingerprint:string -> unit
(** [record bfile ~nonce ~fingerprint] records that a request of [nonce] and
    [fingerprint] wrote into the base [bfile]. To be called while holding the
    lock of the base. The records older than two days are dropped. *)

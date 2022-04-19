
module CacheImpl : sig
  type ('a, 'b) t
  val create : ?random:bool -> int -> ('a,'b) t
  val find_opt : ('a, 'b) t -> 'a -> 'b option
  val add : ('a, 'b) t -> 'a -> 'b -> unit
  val remove : ('a, 'b) t -> 'a -> unit
end = struct
  include Hashtbl
(*  type ('a, 'b) t = ('a, 'b) Hashtbl.t
  let create n = Hashtbl.create ~random:false n*)
  let rec remove t k =
    Hashtbl.remove t k;
    if Hashtbl.mem t k then remove t k
end

(* type of caches *)
type ('a, 'b) t = ('a, 'b) CacheImpl.t

(** [create n] initializes an empty cache, n is the initial size of the cache.
**)
let create = CacheImpl.create


(** [cached ~cache ~f] returns a memoized version of [f] using [cache].
**)
let cached  ~cache ~f =
  let f' arg =
    let value = CacheImpl.find_opt cache arg in
    match value with
    | Some v -> v
    | None ->
       let result = f arg in
       CacheImpl.add cache arg result;
       result
  in f'
       
let invalidate ~cache ~key =
  CacheImpl.remove cache key;
  cache

(** [add ~cache ~key ~value] returns a cache with added pair key value.
 **)  
let add ~cache ~key ~value =
  CacheImpl.add cache key value;
  cache


let find_opt ~cache ~key =
  CacheImpl.find_opt cache key

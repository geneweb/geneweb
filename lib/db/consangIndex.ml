type consang = Unset | Outdated | Deferred | Known of float
type entry = { consang : consang; implexes : int option }
type reader = Closed | Missing | Opened of Unix.file_descr

type t = {
  fname : string;
  lock : string;
  unchanged : unit -> bool;
  buf : Bytes.t;
  mutable reader : reader;
}

let magic = "GnCs0001"
let size = 8
let unset = { consang = Unset; implexes = None }
let outdated = { consang = Outdated; implexes = None }

let make ~unchanged bdir =
  {
    fname = Filename.concat bdir "consang.dat";
    lock = Filename.concat bdir "consang.lck";
    unchanged;
    buf = Bytes.create size;
    reader = Closed;
  }

let close t =
  (match t.reader with Opened fd -> Unix.close fd | Closed | Missing -> ());
  t.reader <- Closed

let remove t =
  close t;
  Mutil.rm t.fname

(* First word of a record: bits 0 and 1, the state of the coefficient, held by
   the second word when known; bit 2, the count held by the upper bits is
   valid. *)
let decode buf =
  let flags = Int32.to_int (Bytes.get_int32_be buf 0) in
  {
    consang =
      (match flags land 3 with
      | 0 -> Unset
      | 1 -> Outdated
      | 2 -> Deferred
      | _ -> Known (Int32.float_of_bits (Bytes.get_int32_be buf 4)));
    implexes = (if flags land 4 <> 0 then Some (flags lsr 3) else None);
  }

let encode buf e =
  let flags, x =
    match e.consang with
    | Unset -> (0, 0.)
    | Outdated -> (1, 0.)
    | Deferred -> (2, 0.)
    | Known x -> (3, x)
  in
  let flags =
    match e.implexes with
    | Some n -> flags lor 4 lor (min n 0xFFFFFFF lsl 3)
    | None -> flags
  in
  Bytes.set_int32_be buf 0 (Int32.of_int flags);
  Bytes.set_int32_be buf 4 (Int32.bits_of_float x)

let filled fd buf =
  let rec loop ofs =
    ofs = size
    ||
    let n = Unix.read fd buf ofs (size - ofs) in
    n > 0 && loop (ofs + n)
  in
  loop 0

let seek fd pos =
  ignore (Unix.LargeFile.lseek fd (Int64.of_int pos) Unix.SEEK_SET : int64)

let has_magic fd buf = filled fd buf && Bytes.to_string buf = magic

let input fd buf i =
  seek fd (size * (i + 1));
  if filled fd buf then decode buf else unset

let output fd buf i e =
  encode buf e;
  seek fd (size * (i + 1));
  ignore (Unix.write fd buf 0 size : int)

let get t i =
  (match t.reader with
  | Closed ->
      t.reader <-
        (match Unix.openfile t.fname [ Unix.O_RDONLY ] 0 with
        | exception Unix.Unix_error _ -> Missing
        | fd when has_magic fd t.buf -> Opened fd
        | fd ->
            Unix.close fd;
            Missing)
  | Missing | Opened _ -> ());
  match t.reader with
  | Opened fd -> input fd t.buf i
  | Closed | Missing -> unset

let write t k =
  let fd = Unix.openfile t.fname [ Unix.O_RDWR; Unix.O_CREAT ] 0o666 in
  Fun.protect ~finally:(fun () ->
      Unix.close fd;
      close t)
  @@ fun () ->
  let buf = Bytes.create size in
  if not (has_magic fd buf) then (
    Unix.ftruncate fd 0;
    seek fd 0;
    ignore (Unix.write_substring fd magic 0 size : int));
  k (input fd buf) (output fd buf)

let update t k =
  Lock.control ~on_exn:Printexc.raise_with_backtrace ~wait:true
    ~lock_file:t.lock (fun () -> write t k)

let cache t k =
  try
    Lock.control
      ~on_exn:(fun _ _ -> ())
      ~wait:false ~lock_file:t.lock
      (fun () -> if t.unchanged () then write t k)
  with Unix.Unix_error _ | Sys_error _ -> ()

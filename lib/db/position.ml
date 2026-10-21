(* OCaml does not provide a native unsigned 32-bit integer type. While
   standard `int` type could hold positions on both 32-bit and 64-bit systems,
   it would require conversions to `int64` for seeking.

   Using `int64` directly is simpler. The amount of wasted memory is acceptable,
   as this representation is not stored on disk and few of them are stored in
   main memory. *)
type t = int64

exception Overflow

let lsb_mask = 0x0000_0000_FFFF_FFFFL
let msb_mask = 0xFFFF_FFFF_0000_0000L

let[@inline] of_int64 (i : int64) =
  if Int64.logand msb_mask i <> 0L then raise Overflow;
  i

let input ic =
  let i = Int64.of_int @@ input_binary_int ic in
  Int64.logand lsb_mask i

let incr p i =
  if i < 0 then invalid_arg "incr";
  of_int64 @@ Int64.(add p (of_int i))

let compare = Int64.compare
let pp ppf = Fmt.pf ppf "%Lx"
let int64_length = 8
let uint32_length = 4

let output =
  let bytes = Bytes.create int64_length in
  fun oc i ->
    Bytes.set_int64_be bytes 0 i;
    output oc bytes 4 uint32_length

let pos_in ic = of_int64 @@ LargeFile.pos_in ic
let seek_in = LargeFile.seek_in
let pos_out oc = of_int64 @@ LargeFile.pos_out oc
let seek_out = LargeFile.seek_out

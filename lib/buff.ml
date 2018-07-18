(* $Id: buff.ml,v 5.1 2006-10-15 15:39:39 ddr Exp $ *)

module Make (B : sig  end) =
  struct
    let buff = ref (Bytes.create 80)
    let store len x =
      if len >= Bytes.length !buff then
        buff := Bytes.extend !buff 0 (Bytes.length !buff);
      Bytes.set !buff len x;
      succ len
    let unsafe_gstore len s si slen =
      let newlen = len + slen in
      if newlen > Bytes.length !buff then
        begin let more = max slen (Bytes.length !buff) in
          buff := Bytes.extend !buff 0 more
        end;
      Bytes.blit_string s si !buff len slen;
      newlen
    let mstore len s = unsafe_gstore len s 0 (String.length s)
    let gstore len s si slen =
      unsafe_gstore len s si (min slen (String.length s - si))
    let get len = Bytes.sub_string !buff 0 len
  end

module BB = Make (struct  end)

let get = BB.get
let store = BB.store
let gstore = BB.gstore
let mstore = BB.mstore
let buff = BB.buff

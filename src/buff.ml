(* $Id: buff.ml,v 5.1 2006-10-15 15:39:39 ddr Exp $ *)

module Make(B : sig end) = (* camlp5 does not support generative functors? *)
  struct
    value buff = ref (Bytes.create 80);
    value store len x =
      do {
        if len >= Bytes.length buff.val then
          buff.val := Bytes.extend buff.val 0 (Bytes.length buff.val)
        else ();
        Bytes.set buff.val len x;
        succ len
      }
    ;
    value unsafe_gstore len s si slen =
      do {
        let newlen = len + slen in
        if newlen > Bytes.length buff.val then
          let more = max slen (Bytes.length buff.val) in
          buff.val := Bytes.extend buff.val 0 more
        else ();
        Bytes.blit_string s si buff.val len slen;
        newlen
      }
    ;
    value mstore len s =
      unsafe_gstore len s 0 (String.length s)
    ;
    value gstore len s si slen =
      unsafe_gstore len s si (min slen (String.length s - si))
    ;
    value get len = Bytes.sub_string buff.val 0 len;
  end
;

module BB = Make(struct end);

value get = BB.get;
value store = BB.store;
value gstore = BB.gstore;
value mstore = BB.mstore;
value buff = BB.buff;

(* $Id: buff.ml,v 5.1 2006-10-15 15:39:39 ddr Exp $ *)

module Make (B : sig value buff : ref string; end) =
  struct
    value buff = B.buff;
    value store len x =
      do {
        if len >= String.length buff.val then
          buff.val := buff.val ^ Bytes.create (String.length buff.val)
        else ();
        Bytes.set buff.val len x;
        succ len
      }
    ;
    value mstore len s =
      add_rec len 0 where rec add_rec len i =
        if i = String.length s then len
        else add_rec (store len s.[i]) (succ i)
    ;
    value gstore len s si slen =
      let iend = si + slen in
      add_rec len si where rec add_rec len i =
        if i = iend || i = String.length s then len
        else add_rec (store len s.[i]) (succ i)
    ;
    value get len = String.sub buff.val 0 len;
  end
;

module BB = Make (struct value buff = ref (Bytes.create 80); end);

value get = BB.get;
value store = BB.store;
value gstore = BB.gstore;
value mstore = BB.mstore;
value buff = BB.buff;

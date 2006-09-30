(* $Id: mutil.ml,v 5.1 2006-09-30 18:07:33 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

value int_size = 4;
value verbose = ref True;
value utf_8_db = Name.utf_8_db;

value rindex s c =
  pos (String.length s - 1) where rec pos i =
    if i < 0 then None else if s.[i] = c then Some i else pos (i - 1)
;

value array_memq x a =
  loop 0 where rec loop i =
    if i == Array.length a then False
    else if x == a.(i) then True
    else loop (i + 1)
;

IFDEF OLD THEN declare
value decline_word case s ibeg iend =
  let i =
    loop ibeg where rec loop i =
      if i + 3 > iend then ibeg
      else if s.[i] == ':' && s.[i + 1] == case && s.[i + 2] == ':' then i + 3
      else loop (i + 1)
  in
  let j =
    loop i where rec loop i =
      if i + 3 > iend then iend
      else if s.[i] == ':' && s.[i + 2] == ':' then i
      else loop (i + 1)
  in
  if i = ibeg then String.sub s ibeg (j - ibeg)
  else if s.[i] == '+' then
    let k =
      loop ibeg where rec loop i =
        if i == iend then i else if s.[i] == ':' then i else loop (i + 1)
    in
    let i = i + 1 in string_sub s ibeg (k - ibeg) ^ string_sub s i (j - i)
  else if s.[i] == '-' then
    let k =
      loop ibeg where rec loop i =
        if i == iend then i else if s.[i] == ':' then i else loop (i + 1)
    in
    let (i, cnt) =
      loop i 0 where rec loop i cnt =
        if i < iend && s.[i] == '-' then
          let cnt =
            loop (cnt + 1) where rec loop cnt =
              if k - cnt = ibeg then cnt
              else if utf_8_intern_byte s.[k - cnt] then loop (cnt + 1)
              else cnt
          in
          loop (i + 1) cnt
        else (i, cnt)
    in
    string_sub s ibeg (k - cnt - ibeg) ^ string_sub s i (j - i)
  else string_sub s i (j - i)
;

value decline case s =
  loop 0 0 where rec loop ibeg i =
    if i == String.length s then
      if i == ibeg then "" else decline_word case s ibeg i
    else
      match s.[i] with
      [ ' ' | '<' | '/' as sep ->
          decline_word case s ibeg i ^ String.make 1 sep ^
            loop (i + 1) (i + 1)
      | '>' -> String.sub s ibeg (i + 1 - ibeg) ^ loop (i + 1) (i + 1)
      | _ -> loop ibeg (i + 1) ]
;
end ELSE declare
(* [decline] has been deprecated since version 5.00
   compatibility code: *)
value colon_to_at_word s ibeg iend =
  let iendroot =
    loop ibeg where rec loop i =
      if i + 3 >= iend then iend
      else if s.[i] = ':' && s.[i+2] = ':' then i
      else loop (i + 1)
  in
  if iendroot = iend then String.sub s ibeg (iend - ibeg)
  else
    let (listdecl, maxd) =
      loop [] 0 iendroot where rec loop list maxd i =
        if i >= iend then (list, maxd)
        else
          let inext =
            loop (i + 3) where rec loop i =
              if i + 3 >= iend then iend
              else if s.[i] = ':' && s.[i+2] = ':' then i
              else loop (i + 1)
          in
          let (e, d) =
            let i = i + 3 in
            let j = inext in
            if i < j && s.[i] = '+' then
              (String.sub s (i + 1) (j - i - 1), 0)
            else if i < j && s.[i] = '-' then
              loop 1 (i + 1) where rec loop n i =
                if i < j && s.[i] = '-' then loop (n + 1) (i + 1)
                else (String.sub s i (j - i), n)
            else (String.sub s i (j - i), iendroot - ibeg)
          in
          loop [(s.[i+1], e, d) :: list] (max d maxd) inext
    in
    let len = max 0 (iendroot - ibeg - maxd) in
    let root = String.sub s ibeg len in
    let s =
      List.fold_left
        (fun t (c, e, d) ->
           Printf.sprintf "%c?%s%s" c e (if t = "" then "" else ":" ^ t))
        (String.sub s (ibeg + len) (iendroot - ibeg - len)) listdecl
    in
    root ^ "@(" ^ s ^ ")"
;

value colon_to_at s =
  loop 0 0 where rec loop ibeg i =
    if i == String.length s then
      if i == ibeg then "" else colon_to_at_word s ibeg i
    else
      match s.[i] with
      [ ' ' | '<' | '/' as sep ->
          colon_to_at_word s ibeg i ^ String.make 1 sep ^ loop (i + 1) (i + 1)
      | '>' -> String.sub s ibeg (i + 1 - ibeg) ^ loop (i + 1) (i + 1)
      | _ -> loop ibeg (i + 1) ]
;

value decline case s =
  Printf.sprintf "@(@(%c)%s)" case
    (if not (String.contains s ':') then s else colon_to_at s)
;
(* end compatibility code *)
end END;

value nominative s =
  match rindex s ':' with
  [ Some _ -> decline 'n' s
  | _ -> s ]
;

value remove_file f = try Sys.remove f with [ Sys_error _ -> () ];

value mkdir_p x =
  loop x where rec loop x =
    do  {
      let y = Filename.dirname x;
      if y <> x && String.length y < String.length x then loop y else ();
      try Unix.mkdir x 0o755 with [ Unix.Unix_error _ _ _ -> () ];
    }
;

value rec remove_dir d =
  do {
    try
      let files = Sys.readdir d in
      for i = 0 to Array.length files - 1 do {
        remove_dir (Filename.concat d files.(i));
        remove_file (Filename.concat d files.(i));
      }
    with
    [ Sys_error _ -> () ];
    try Unix.rmdir d with [ Unix.Unix_error _ _ _ -> () ];
  }
;

value output_value_no_sharing oc v =
  Marshal.to_channel oc v [Marshal.No_sharing]
;

value initial n =
  loop 0 where rec loop i =
    if i == String.length n then 0
    else
      match n.[i] with
      [ 'A'..'Z' | 'À'..'Ý' -> i
      | _ -> loop (succ i) ]
;

value input_particles fname =
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      loop [] 0 where rec loop list len =
        match try Some (input_char ic) with [ End_of_file -> None ] with
        [ Some '_' -> loop list (Buff.store len ' ')
        | Some '\n' ->
            loop (if len = 0 then list else [Buff.get len :: list]) 0
        | Some '\r' -> loop list len
        | Some c -> loop list (Buff.store len c)
        | None ->
            do {
              close_in ic;
              List.rev (if len = 0 then list else [Buff.get len :: list])
            } ]
  | None -> [] ]
;

value saints = ["saint"; "sainte"];

value surnames_pieces surname =
  let surname = Name.lower surname in
  let flush i0 i1 =
    if i1 > i0 then [String.sub surname i0 (i1 - i0)] else []
  in
  let rec loop i0 iw i =
    if i == String.length surname then
      if i0 == 0 then [] else if i > i0 + 3 then flush i0 i else []
    else if surname.[i] == ' ' then
      if i > iw + 3 then
        let w = String.sub surname iw (i - iw) in
        if List.mem w saints then loop i0 (i + 1) (i + 1)
        else flush i0 i @ loop (i + 1) (i + 1) (i + 1)
      else loop i0 (i + 1) (i + 1)
    else loop i0 iw (i + 1)
  in
  loop 0 0 0
;

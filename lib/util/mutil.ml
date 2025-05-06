(* Copyright (c) 2006-2007 INRIA *)

let bench name fn =
  let pprint_gc gc =
    let open Gc in
    let pint x =
      let s = string_of_int x in
      let aux i = String.make 1 @@ String.unsafe_get s i in
      let rec loop i n acc =
        if i < 0 then String.concat "" (if x > 0 then "+" :: acc else acc)
        else
          let acc =
            if n > 0 && n mod 3 = 0 && n <> 1 && String.unsafe_get s 0 <> '-'
            then aux i :: "," :: acc
            else aux i :: acc
          in
          loop (i - 1) (n + 1) acc
      in
      loop (String.length s - 1) 0 []
    in
    Printf.printf
      "\tminor_words : %s\n\
       \tpromoted_words : %s\n\
       \tmajor_words : %s\n\
       \tminor_collections : %s\n\
       \tmajor_collections : %s\n\
       \theap_words : %s\n\
       \theap_chunks : %s\n\
       \tlive_words : %s\n\
       \tlive_blocks : %s\n\
       \tfree_words : %s\n\
       \tfree_blocks : %s\n\
       \tlargest_free : %s\n\
       \tfragments : %s\n\
       \tcompactions : %s\n\
       \ttop_heap_words : %s\n\
       \tstack_size : %s\n"
      (gc.minor_words |> truncate |> pint)
      (gc.promoted_words |> truncate |> pint)
      (gc.major_words |> truncate |> pint)
      (gc.minor_collections |> pint)
      (gc.major_collections |> pint)
      (gc.heap_words |> pint) (gc.heap_chunks |> pint) (gc.live_words |> pint)
      (gc.live_blocks |> pint) (gc.free_words |> pint) (gc.free_blocks |> pint)
      (gc.largest_free |> pint) (gc.fragments |> pint) (gc.compactions |> pint)
      (gc.top_heap_words |> pint)
      (gc.stack_size |> pint)
  in
  (* OCaml 4.12 added [forced_major_collections] field. *)
  (* Using [@warning "-23"] and "gc1 with" as a workaround. *)
  let[@warning "-23"] diff gc1 gc2 =
    Gc.
      {
        gc1 with
        minor_words = gc2.minor_words -. gc1.minor_words;
        promoted_words = gc2.promoted_words -. gc1.promoted_words;
        major_words = gc2.major_words -. gc1.major_words;
        minor_collections = gc2.minor_collections - gc1.minor_collections;
        major_collections = gc2.major_collections - gc1.major_collections;
        heap_words = gc2.heap_words - gc1.heap_words;
        heap_chunks = gc2.heap_chunks - gc1.heap_chunks;
        live_words = gc2.live_words - gc1.live_words;
        live_blocks = gc2.live_blocks - gc1.live_blocks;
        free_words = gc2.free_words - gc1.free_words;
        free_blocks = gc2.free_blocks - gc1.free_blocks;
        largest_free = gc2.largest_free - gc1.largest_free;
        fragments = gc2.fragments - gc1.fragments;
        compactions = gc2.compactions - gc1.compactions;
        top_heap_words = gc2.top_heap_words - gc1.top_heap_words;
        stack_size = gc2.stack_size - gc1.stack_size;
      }
  in
  let gc1 = Gc.stat () in
  let p1 = Sys.time () in
  let t1 = Unix.gettimeofday () in
  let res = fn () in
  let t2 = Unix.gettimeofday () in
  let p2 = Sys.time () in
  let gc2 = Gc.stat () in
  Printf.printf "[%s]: %fs (~%fs CPU)\n" name (t2 -. t1) (p2 -. p1);
  pprint_gc (diff gc1 gc2);
  res

let print_callstack ?(max = 5) () =
  Printexc.(print_raw_backtrace stderr @@ get_callstack max)

let verbose = ref true
let rm fname = if Sys.file_exists fname then Sys.remove fname
let mv src dst = if Sys.file_exists src then Sys.rename src dst

let list_iter_first f = function
  | [] -> ()
  | hd :: tl ->
      f true hd;
      List.iter (f false) tl

(* [decline] has been deprecated since version 5.00
   compatibility code: *)
let colon_to_at_word s ibeg iend =
  let iendroot =
    let rec loop i =
      if i + 3 >= iend then iend
      else if s.[i] = ':' && s.[i + 2] = ':' then i
      else loop (i + 1)
    in
    loop ibeg
  in
  if iendroot = iend then String.sub s ibeg (iend - ibeg)
  else
    let listdecl, maxd =
      let rec loop list maxd i =
        if i >= iend then (list, maxd)
        else
          let inext =
            let rec loop i =
              if i + 3 >= iend then iend
              else if s.[i] = ':' && s.[i + 2] = ':' then i
              else loop (i + 1)
            in
            loop (i + 3)
          in
          let e, d =
            let i = i + 3 in
            let j = inext in
            if i < j && s.[i] = '+' then (String.sub s (i + 1) (j - i - 1), 0)
            else if i < j && s.[i] = '-' then
              let rec loop n i =
                if i < j && s.[i] = '-' then loop (n + 1) (i + 1)
                else (String.sub s i (j - i), n)
              in
              loop 1 (i + 1)
            else (String.sub s i (j - i), iendroot - ibeg)
          in
          loop ((s.[i + 1], e) :: list) (max d maxd) inext
      in
      loop [] 0 iendroot
    in
    let len = max 0 (iendroot - ibeg - maxd) in
    let root = String.sub s ibeg len in
    let s =
      List.fold_left
        (fun t (c, e) ->
          Printf.sprintf "%c?%s%s" c e (if t = "" then "" else ":" ^ t))
        (String.sub s (ibeg + len) (iendroot - ibeg - len))
        listdecl
    in
    root ^ "@(" ^ s ^ ")"

let colon_to_at s =
  let rec loop ibeg i =
    if i = String.length s then
      if i = ibeg then "" else colon_to_at_word s ibeg i
    else
      match s.[i] with
      | (' ' | '<' | '/') as sep ->
          colon_to_at_word s ibeg i ^ String.make 1 sep ^ loop (i + 1) (i + 1)
      | '>' -> String.sub s ibeg (i + 1 - ibeg) ^ loop (i + 1) (i + 1)
      | _ -> loop ibeg (i + 1)
  in
  loop 0 0

let decline case s =
  Printf.sprintf "@(@(%c)%s)" case
    (if not (String.contains s ':') then s else colon_to_at s)
(* end compatibility code *)

let nominative s =
  match String.rindex_opt s ':' with Some _ -> decline 'n' s | _ -> s

let mkdir_p ?(perm = 0o755) d =
  let rec loop d =
    let d1 = Filename.dirname d in
    if d1 <> d && String.length d1 < String.length d then loop d1;
    if not (Sys.file_exists d) then
      try Unix.mkdir d perm
      with Unix.Unix_error (_, _, _) -> Printf.eprintf "Failed mkdir: %s\n" d
  in
  loop d

let lock_file bname = Filename.remove_extension bname ^ ".lck"

let initial n =
  let rec loop i =
    if i = String.length n then 0
    else
      match n.[i] with 'A' .. 'Z' | '\192' .. '\221' -> i | _ -> loop (succ i)
  in
  loop 0

let default_particles =
  let upper =
    [
      "AF ";
      "D'";
      "D’";
      "DAL ";
      "DE ";
      "DES ";
      "DI ";
      "DU ";
      "OF ";
      "VAN ";
      "VON UND ZU ";
      "VON ";
      "Y ";
      "ZU ";
      "ZUR ";
    ]
  in
  List.rev_append (List.rev_map String.lowercase_ascii upper) upper

let input_particles fname =
  try
    let ic = open_in fname in
    let rec loop list len =
      match input_char ic with
      | '_' -> loop list (Buff.store len ' ')
      | '\n' -> loop (if len = 0 then list else Buff.get len :: list) 0
      | '\r' -> loop list len
      | c -> loop list (Buff.store len c)
      | exception End_of_file ->
          close_in ic;
          List.rev (if len = 0 then list else Buff.get len :: list)
    in
    loop [] 0
  with Sys_error _ -> default_particles

let saints = [ "saint"; "sainte" ]

let surnames_pieces surname =
  let surname = Name.lower surname in
  let flush i0 i1 =
    if i1 > i0 then [ String.sub surname i0 (i1 - i0) ] else []
  in
  let rec loop i0 iw i =
    if i = String.length surname then
      if i0 = 0 then [] else if i > i0 + 3 then flush i0 i else []
    else if surname.[i] = ' ' then
      if i > iw + 3 then
        let w = String.sub surname iw (i - iw) in
        if List.mem w saints then loop i0 (i + 1) (i + 1)
        else flush i0 i @ loop (i + 1) (i + 1) (i + 1)
      else loop i0 (i + 1) (i + 1)
    else loop i0 iw (i + 1)
  in
  loop 0 0 0

let tr c1 c2 s =
  match String.rindex_opt s c1 with
  | Some _ ->
      String.init (String.length s) (fun i ->
          let c = String.unsafe_get s i in
          if c = c1 then c2 else c)
  | None -> s

let unsafe_tr c1 c2 s =
  match String.rindex_opt s c1 with
  | Some _ ->
      let bytes = Bytes.unsafe_of_string s in
      for i = 0 to Bytes.length bytes - 1 do
        if Bytes.unsafe_get bytes i = c1 then Bytes.unsafe_set bytes i c2
      done;
      Bytes.unsafe_to_string bytes
  | None -> s

let utf_8_of_iso_8859_1 str =
  let rec loop i len =
    if i = String.length str then Buff.get len
    else
      let c = str.[i] in
      if Char.code c < 0x80 then loop (i + 1) (Buff.store len c)
      else if Char.code c < 0xC0 then
        let len = Buff.store len (Char.chr 0xC2) in
        loop (i + 1) (Buff.store len c)
      else
        let len = Buff.store len (Char.chr 0xC3) in
        loop (i + 1) (Buff.store len (Char.chr (Char.code c - 0x40)))
  in
  loop 0 0

let iso_8859_1_of_utf_8 s =
  let rec loop i len =
    if i = String.length s then Buff.get len
    else
      let c = s.[i] in
      match Char.code c with
      | 0xC2 when i + 1 < String.length s ->
          loop (i + 2) (Buff.store len s.[i + 1])
      | 0xC3 when i + 1 < String.length s ->
          loop (i + 2) (Buff.store len (Char.chr (Char.code s.[i + 1] + 0x40)))
      | _ -> loop (i + 1) (Buff.store len c)
  in
  loop 0 0

let strip_all_trailing_spaces s =
  let b = Buffer.create (String.length s) in
  let len =
    let rec loop i =
      if i < 0 then 0
      else
        match s.[i] with ' ' | '\t' | '\r' | '\n' -> loop (i - 1) | _ -> i + 1
    in
    loop (String.length s - 1)
  in
  let rec loop i =
    if i = len then Buffer.contents b
    else
      match s.[i] with
      | '\r' -> loop (i + 1)
      | ' ' | '\t' ->
          let rec loop0 j =
            if j = len then Buffer.contents b
            else
              match s.[j] with
              | ' ' | '\t' | '\r' -> loop0 (j + 1)
              | '\n' -> loop j
              | _ ->
                  Buffer.add_char b s.[i];
                  loop (i + 1)
          in
          loop0 (i + 1)
      | c ->
          Buffer.add_char b c;
          loop (i + 1)
  in
  loop 0

let roman_of_arabian n =
  let build one five ten = function
    | 0 -> ""
    | 1 -> one
    | 2 -> one ^ one
    | 3 -> one ^ one ^ one
    | 4 -> one ^ five
    | 5 -> five
    | 6 -> five ^ one
    | 7 -> five ^ one ^ one
    | 8 -> five ^ one ^ one ^ one
    | _ -> one ^ ten
  in
  build "M" "M" "M" (n / 1000 mod 10)
  ^ build "C" "D" "M" (n / 100 mod 10)
  ^ build "X" "L" "C" (n / 10 mod 10)
  ^ build "I" "V" "X" (n mod 10)

let arabian_of_roman s =
  let decode_digit one five ten r =
    let rec loop cnt i =
      if i >= String.length s then ((10 * r) + cnt, i)
      else if s.[i] = one then loop (cnt + 1) (i + 1)
      else if s.[i] = five then
        if cnt = 0 then loop 5 (i + 1) else ((10 * r) + 5 - cnt, i + 1)
      else if s.[i] = ten then ((10 * r) + 10 - cnt, i + 1)
      else ((10 * r) + cnt, i)
    in
    loop 0
  in
  let r, i = decode_digit 'M' 'M' 'M' 0 0 in
  let r, i = decode_digit 'C' 'D' 'M' r i in
  let r, i = decode_digit 'X' 'L' 'C' r i in
  let r, i = decode_digit 'I' 'V' 'X' r i in
  if i = String.length s then r else raise Not_found

module StrSet = Set.Make (struct
  type t = string

  let compare = compare
end)

let start_with ini i s =
  let inilen = String.length ini in
  let strlen = String.length s in
  if i < 0 || i > strlen then raise (Invalid_argument "start_with");
  let rec loop i1 i2 =
    if i1 = inilen then true
    else if i2 = strlen then false
    else if String.unsafe_get s i2 = String.unsafe_get ini i1 then
      loop (i1 + 1) (i2 + 1)
    else false
  in
  loop 0 i

let start_with_wildcard ini i s =
  let inilen = String.length ini in
  let strlen = String.length s in
  if i < 0 || i > strlen then raise (Invalid_argument "start_with_wildcard");
  let rec loop i1 i2 =
    if i1 = inilen then true
    else if i2 = strlen then
      if String.unsafe_get ini i1 = '_' then loop (i1 + 1) i2 else false
    else if
      String.unsafe_get s i2 = String.unsafe_get ini i1
      || (String.unsafe_get s i2 = ' ' && String.unsafe_get ini i1 = '_')
    then loop (i1 + 1) (i2 + 1)
    else false
  in
  loop 0 i

let contains str sub =
  let strlen = String.length str in
  let sublen = String.length sub in
  let rec aux i1 i2 =
    if i1 = sublen then true
    else if i2 = strlen then false
    else if String.unsafe_get str i2 = String.unsafe_get sub i1 then
      aux (i1 + 1) (i2 + 1)
    else false
  in
  let rec loop i =
    if i + sublen <= strlen then aux 0 i || loop (i + 1) else false
  in
  loop 0

let compile_particles list =
  let parts =
    list
    |> List.map (fun s -> Re.str (tr '_' ' ' s))
    |> Re.alt |> Re.longest |> Re.group
  in
  Re.(seq [ bos; parts; greedy (rep notnl) ]) |> Re.compile

let get_particle re s =
  match Re.exec_opt re s with Some g -> Re.Group.get g 1 | None -> ""

let compare_after_particle particles s1 s2 =
  let p1 = get_particle particles s1 in
  let p2 = get_particle particles s2 in
  let rec loop i1 i2 =
    if i1 = String.length s1 && i2 = String.length s2 then compare p1 p2
    else if i1 = String.length s1 then -1
    else if i2 = String.length s2 then 1
    else
      let c1 = String.unsafe_get s1 i1 in
      let c2 = String.unsafe_get s2 i2 in
      if c1 < c2 then -1 else if c1 > c2 then 1 else loop (i1 + 1) (i2 + 1)
  in
  loop (String.length p1) (String.length p2)

let hold = ref ""
let fallback = ref []

let read_fallback lang fname =
  fallback := [];
  let rec aux a b i =
    i = -1 || (String.unsafe_get a i = String.unsafe_get b i && aux a b (i - 1))
  in
  let ic = try Some (Secure.open_in fname) with Sys_error _ -> None in
  match ic with
  | Some ic ->
      let rec one_line () =
        match input_line ic with
        | exception End_of_file -> close_in ic
        | line -> (
            let lang_len = String.length lang in
            match String.index_opt line ':' with
            | Some i
              when line.[0] <> '#'
                   && i = lang_len
                   && aux lang line (lang_len - 1) ->
                let f_lang =
                  String.sub line (i + 1) (String.length line - i - 1)
                in
                fallback := (lang, f_lang) :: !fallback;
                one_line ()
            | _ -> one_line ())
      in
      one_line ()
  | None ->
      fallback := [ ("co", "fr"); ("oc", "fr"); ("br", "fr"); ("bg", "ru") ]

let input_lexicon lang ht open_fname =
  read_fallback lang "lexicon.gwf";
  let ic = open_fname () in
  let lang =
    match String.index_opt lang '.' with
    | Some i -> String.sub lang 0 i
    | None -> lang
  in
  let lang_len = String.length lang in
  let derived_lang =
    match String.index_opt lang '-' with
    | Some i -> String.sub lang 0 i
    | None -> (
        match String.index_opt lang '_' with
        | Some i -> String.sub lang 0 i
        | None -> "")
  in
  let derived_lang_len = String.length derived_lang in
  let rec aux a b i =
    i = -1 || (String.unsafe_get a i = String.unsafe_get b i && aux a b (i - 1))
  in
  (* find header *)
  let tmp = Hashtbl.create 0 in
  let rec key () =
    match input_line ic with
    | exception End_of_file -> close_in ic
    | line ->
        let len = String.length line in
        if len < 4 then key ()
        else if
          String.unsafe_get line 0 = ' '
          && String.unsafe_get line 1 = ' '
          && String.unsafe_get line 2 = ' '
          && String.unsafe_get line 3 = ' '
        then trad (String.sub line 4 (len - 4))
        else key ()
  (* find a line corresponding to a language *)
  and trad k =
    hold := "";
    match input_line ic with
    | exception End_of_file -> close_in ic
    | line -> (
        match String.index_opt line ':' with
        | Some i
          when (i = lang_len && aux lang line (lang_len - 1))
               || i = derived_lang_len
                  && aux derived_lang line (derived_lang_len - 1) ->
            let v =
              if i + 1 = String.length line then ""
              else String.sub line (i + 2) (String.length line - i - 2)
            in
            Hashtbl.replace ht k v;
            trad k
        | Some i
          when List.mem_assoc lang !fallback
               && i = String.length (List.assoc lang !fallback)
               && aux
                    (List.assoc lang !fallback)
                    line
                    (String.length (List.assoc lang !fallback) - 1) ->
            let v =
              if i + 1 = String.length line then ""
              else String.sub line (i + 2) (String.length line - i - 2)
            in
            hold := v;
            trad k
        | Some _i
          when String.length line > 4
               && String.unsafe_get line 0 = '-'
               && String.unsafe_get line 1 = '>'
               && String.unsafe_get line 2 = ':'
               && String.unsafe_get line 3 = ' ' ->
            (* defining alias names for existing entries in the lexicon *)
            (*     alias_name *)
            (* ->: real_entry *)
            let k2 = String.sub line 4 (String.length line - 4) in
            Hashtbl.replace tmp k k2;
            trad k
        | Some _i -> trad k
        | None ->
            if (not (Hashtbl.mem ht k)) && !hold <> "" then
              Hashtbl.add ht k !hold;
            key ())
  in
  key ();
  Hashtbl.iter
    (fun k k2 ->
      match Hashtbl.find_opt ht k2 with
      | Some entry -> Hashtbl.replace ht k entry
      | None -> ())
    tmp

let array_to_list_map fn a = Array.fold_right (fun x acc -> fn x :: acc) a []
let array_to_list_rev_map fn a = Array.fold_left (fun acc x -> fn x :: acc) [] a

let array_assoc k a =
  let len = Array.length a in
  let rec loop i =
    if i = len then raise Not_found
    else
      let k', v = Array.unsafe_get a i in
      if k' = k then v else loop (i + 1)
  in
  loop 0

let string_of_int_sep sep x =
  let digits, len =
    let rec loop (d, l) x =
      if x = 0 then (d, l)
      else loop (Char.chr (Char.code '0' + (x mod 10)) :: d, l + 1) (x / 10)
    in
    loop ([], 0) x
  in
  let digits, len = if digits = [] then ([ '0' ], 1) else (digits, len) in
  let slen = String.length sep in
  let s = Bytes.create (len + ((len - 1) / 3 * slen)) in
  let _ =
    List.fold_left
      (fun (i, j) c ->
        Bytes.set s j c;
        if i < len - 1 && (len - 1 - i) mod 3 = 0 then (
          String.blit sep 0 s (j + 1) slen;
          (i + 1, j + 1 + slen))
        else (i + 1, j + 1))
      (0, 0) digits
  in
  Bytes.unsafe_to_string s

let rec list_compare cmp l1 l2 =
  match (l1, l2) with
  | x1 :: l1, x2 :: l2 -> (
      match cmp x1 x2 with 0 -> list_compare cmp l1 l2 | x -> x)
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1

let rec list_find_map f = function
  | [] -> None
  | x :: l -> (
      match f x with Some _ as result -> result | None -> list_find_map f l)

let array_find_map f a =
  let n = Array.length a in
  let rec loop i =
    if i = n then None
    else
      match f (Array.unsafe_get a i) with
      | None -> loop (succ i)
      | Some _ as r -> r
  in
  loop 0

let rec list_last = function
  | [] -> raise (Failure "list_last")
  | [ x ] -> x
  | _ :: tl -> list_last tl

let list_ref_append tl hd = tl := hd :: !tl

let executable_magic =
  match Sys.getenv_opt "GW_EXECUTABLE_MAGIC" with
  | Some x -> x
  | None -> Digest.file Sys.executable_name

let random_magic =
  Random.self_init ();
  Random.bits () |> string_of_int

let check_magic magic ic =
  let len = String.length magic in
  let pos = pos_in ic in
  if in_channel_length ic - pos < len then false
  else if magic = really_input_string ic len then true
  else (
    seek_in ic pos;
    false)

let array_except v a =
  let rec loop i =
    if i = Array.length a then Array.copy a
    else if a.(i) = v then
      Array.append (Array.sub a 0 i)
        (Array.sub a (i + 1) (Array.length a - i - 1))
    else loop (i + 1)
  in
  loop 0

let array_forall2 f a1 a2 =
  if Array.length a1 <> Array.length a2 then invalid_arg "array_forall2"
  else
    let rec loop i =
      if i = Array.length a1 then true
      else if f a1.(i) a2.(i) then loop (i + 1)
      else false
    in
    loop 0

let rec list_replace old_v new_v = function
  | [] -> []
  | hd :: tl ->
      if hd = old_v then new_v :: tl else hd :: list_replace old_v new_v tl

let list_except x =
  let rec loop acc = function
    | [] -> []
    | hd :: tl -> if hd = x then List.rev_append acc tl else loop (hd :: acc) tl
  in
  loop []

let list_index x list =
  let rec loop i = function
    | [] -> raise Not_found
    | hd :: tl -> if hd = x then i else loop (succ i) tl
  in
  loop 0 list

let list_slice a b list =
  let rec list_slice a b = function
    | [] -> []
    | hd :: tl ->
        if a <> 0 then list_slice (pred a) b tl
        else if b <> 0 then hd :: list_slice 0 (pred b) tl
        else []
  in
  list_slice a (b - a) list

let input_file_ic ic =
  let len = in_channel_length ic in
  if Sys.unix then (
    let bytes = Bytes.create len in
    really_input ic bytes 0 len;
    Bytes.unsafe_to_string bytes)
  else if len = 0 then ""
  else
    let buffer = Buffer.create len in
    let rec loop () =
      match input_line ic with
      | line ->
          Buffer.add_string buffer line;
          let pos = pos_in ic in
          if
            pos < len
            || (seek_in ic @@ (pos - 1);
                input_char ic)
               = '\n'
          then Buffer.add_char buffer '\n';
          loop ()
      | exception End_of_file -> Buffer.contents buffer
    in
    loop ()

let normalize_utf_8 s =
  let b = Buffer.create (String.length s * 3) in
  let n = Uunf.create `NFC in
  let rec add v =
    match Uunf.add n v with
    | `Uchar u ->
        Uutf.Buffer.add_utf_8 b u;
        add `Await
    | `Await | `End -> ()
  in
  let add_uchar _ _ = function
    | `Malformed _ -> add (`Uchar Uutf.u_rep)
    | `Uchar _ as u -> add u
  in
  Uutf.String.fold_utf_8 add_uchar () s;
  add `End;
  Buffer.contents b

(* Copied from OCaml's List.sort_uniq and adapted to our needs
   (commit e5ebec7 from Nov 7, 2019) *)
let list_map_sort_uniq (fn : 'a -> 'b) l =
  let open List in
  let rec rev_merge l1 l2 accu =
    match (l1, l2) with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1 :: t1, h2 :: t2 ->
        let c = Stdlib.compare h1 h2 in
        if c = 0 then rev_merge t1 t2 (h1 :: accu)
        else if c < 0 then rev_merge t1 l2 (h1 :: accu)
        else rev_merge l1 t2 (h2 :: accu)
  in
  let rec rev_merge_rev l1 l2 accu =
    match (l1, l2) with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1 :: t1, h2 :: t2 ->
        let c = Stdlib.compare h1 h2 in
        if c = 0 then rev_merge_rev t1 t2 (h1 :: accu)
        else if c > 0 then rev_merge_rev t1 l2 (h1 :: accu)
        else rev_merge_rev l1 t2 (h2 :: accu)
  in
  let rec sort n l =
    match (n, l) with
    | 2, x1 :: x2 :: tl ->
        let x1 = fn x1 in
        let x2 = fn x2 in
        let s =
          let c = Stdlib.compare x1 x2 in
          if c = 0 then [ x1 ] else if c < 0 then [ x1; x2 ] else [ x2; x1 ]
        in
        (s, tl)
    | 3, x1 :: x2 :: x3 :: tl ->
        let x1 = fn x1 in
        let x2 = fn x2 in
        let x3 = fn x3 in
        let s =
          let c = Stdlib.compare x1 x2 in
          if c = 0 then
            let c = Stdlib.compare x2 x3 in
            if c = 0 then [ x2 ] else if c < 0 then [ x2; x3 ] else [ x3; x2 ]
          else if c < 0 then
            let c = Stdlib.compare x2 x3 in
            if c = 0 then [ x1; x2 ]
            else if c < 0 then [ x1; x2; x3 ]
            else
              let c = Stdlib.compare x1 x3 in
              if c = 0 then [ x1; x2 ]
              else if c < 0 then [ x1; x3; x2 ]
              else [ x3; x1; x2 ]
          else
            let c = Stdlib.compare x1 x3 in
            if c = 0 then [ x2; x1 ]
            else if c < 0 then [ x2; x1; x3 ]
            else
              let c = Stdlib.compare x2 x3 in
              if c = 0 then [ x2; x1 ]
              else if c < 0 then [ x2; x3; x1 ]
              else [ x3; x2; x1 ]
        in
        (s, tl)
    | n, l ->
        let n1 = n asr 1 in
        let n2 = n - n1 in
        let s1, l2 = rev_sort n1 l in
        let s2, tl = rev_sort n2 l2 in
        (rev_merge_rev s1 s2 [], tl)
  and rev_sort n l =
    match (n, l) with
    | 2, x1 :: x2 :: tl ->
        let x1 = fn x1 in
        let x2 = fn x2 in
        let s =
          let c = Stdlib.compare x1 x2 in
          if c = 0 then [ x1 ] else if c > 0 then [ x1; x2 ] else [ x2; x1 ]
        in
        (s, tl)
    | 3, x1 :: x2 :: x3 :: tl ->
        let x1 = fn x1 in
        let x2 = fn x2 in
        let x3 = fn x3 in
        let s =
          let c = Stdlib.compare x1 x2 in
          if c = 0 then
            let c = Stdlib.compare x2 x3 in
            if c = 0 then [ x2 ] else if c > 0 then [ x2; x3 ] else [ x3; x2 ]
          else if c > 0 then
            let c = Stdlib.compare x2 x3 in
            if c = 0 then [ x1; x2 ]
            else if c > 0 then [ x1; x2; x3 ]
            else
              let c = Stdlib.compare x1 x3 in
              if c = 0 then [ x1; x2 ]
              else if c > 0 then [ x1; x3; x2 ]
              else [ x3; x1; x2 ]
          else
            let c = Stdlib.compare x1 x3 in
            if c = 0 then [ x2; x1 ]
            else if c > 0 then [ x2; x1; x3 ]
            else
              let c = Stdlib.compare x2 x3 in
              if c = 0 then [ x2; x1 ]
              else if c > 0 then [ x2; x3; x1 ]
              else [ x3; x2; x1 ]
        in
        (s, tl)
    | n, l ->
        let n1 = n asr 1 in
        let n2 = n - n1 in
        let s1, l2 = sort n1 l in
        let s2, tl = sort n2 l2 in
        (rev_merge s1 s2 [], tl)
  in
  let len = length l in
  if len < 2 then List.map fn l else fst (sort len l)

let list_rev_map_append f l1 l2 =
  let rec aux acc = function [] -> acc | hd :: tl -> aux (f hd :: acc) tl in
  aux l2 l1

let rec list_rev_iter f = function
  | [] -> ()
  | hd :: tl ->
      list_rev_iter f tl;
      f hd

(* POSIX lockf(3), and fcntl(2), releases its locks when the process
   that holds the locks closes ANY file descriptor that was open on that file.
*)
let read_or_create_channel ?magic ?(wait = false) fname read write =
  if not Sys.unix then ignore wait;

  assert (Secure.check fname);
  let fd = Unix.openfile fname [ Unix.O_RDWR; Unix.O_CREAT ] 0o666 in

  if Sys.unix then (
    try Unix.lockf fd (if wait then Unix.F_LOCK else Unix.F_TLOCK) 0
    with e ->
      Unix.close fd;
      raise e);

  let ic = Unix.in_channel_of_descr fd in
  let read () =
    seek_in ic 0;
    try
      match magic with
      | Some m when check_magic m ic ->
          let r = Some (read ic) in
          let _ = seek_in ic (in_channel_length ic - String.length m) in
          assert (check_magic m ic);
          r
      | Some _ -> None
      | None -> Some (read ic)
    with _ -> None
  in
  match read () with
  | Some v ->
      if Sys.unix then Unix.lockf fd Unix.F_ULOCK 0;
      close_in ic;
      v
  | None ->
      Unix.ftruncate fd 0;
      let oc = Unix.out_channel_of_descr fd in
      seek_out oc 0;
      (match magic with Some m -> seek_out oc (String.length m) | None -> ());
      let v = write oc in
      flush oc;
      let _ = seek_out oc (out_channel_length oc) in
      (match magic with Some m -> output_string oc m | None -> ());
      (match magic with
      | Some m ->
          seek_out oc 0;
          output_string oc m
      | None -> ());
      flush oc;
      if Sys.unix then Unix.lockf fd Unix.F_ULOCK 0;
      close_out oc;
      v

let read_or_create_value ?magic ?wait fname create =
  let read ic = Marshal.from_channel ic in
  let write oc =
    let v = create () in
    Marshal.to_channel oc v [ Marshal.No_sharing; Marshal.Closures ];
    v
  in
  try read_or_create_channel ?magic ?wait fname read write with _ -> create ()

let encode s : Adef.encoded_string =
  let special = function
    | '\000' .. '\031'
    | '\127' .. '\255'
    | '<' | '>' | '"' | '#' | '%' | '{' | '}' | '|' | '\\' | '^' | '~' | '['
    | ']' | '`' | ';' | '/' | '?' | ':' | '@' | '=' | '&' | '+' ->
        true
    | _ -> false
  in
  let hexa_digit x =
    if x >= 10 then Char.chr (Char.code 'A' + x - 10)
    else Char.chr (Char.code '0' + x)
  in
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
      | ' ' -> true
      | x -> if special x then true else need_code (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i1 = if special s.[i] then i1 + 3 else succ i1 in
      compute_len (succ i) i1
    else i1
  in
  let rec copy_code_in s1 i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
        | ' ' ->
            Bytes.set s1 i1 '+';
            succ i1
        | c ->
            if special c then (
              Bytes.set s1 i1 '%';
              Bytes.set s1 (i1 + 1) (hexa_digit (Char.code c / 16));
              Bytes.set s1 (i1 + 2) (hexa_digit (Char.code c mod 16));
              i1 + 3)
            else (
              Bytes.set s1 i1 c;
              succ i1)
      in
      copy_code_in s1 (succ i) i1
    else Bytes.unsafe_to_string s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in
    Adef.encoded (copy_code_in (Bytes.create len) 0 0)
  else Adef.encoded s

let gen_decode strip_spaces (s : Adef.encoded_string) : string =
  let s = (s :> string) in
  let hexa_val conf =
    match conf with
    | '0' .. '9' -> Char.code conf - Char.code '0'
    | 'a' .. 'f' -> Char.code conf - Char.code 'a' + 10
    | 'A' .. 'F' -> Char.code conf - Char.code 'A' + 10
    | _ -> 0
  in
  let rec need_decode i =
    if i < String.length s then
      match s.[i] with '%' | '+' -> true | _ -> need_decode (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
        | '%' when i + 2 < String.length s -> i + 3
        | _ -> succ i
      in
      compute_len i (succ i1)
    else i1
  in
  let rec copy_decode_in s1 i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
        | '%' when i + 2 < String.length s ->
            let v = (hexa_val s.[i + 1] * 16) + hexa_val s.[i + 2] in
            Bytes.set s1 i1 (Char.chr v);
            i + 3
        | '+' ->
            Bytes.set s1 i1 ' ';
            succ i
        | x ->
            Bytes.set s1 i1 x;
            succ i
      in
      copy_decode_in s1 i (succ i1)
    else Bytes.unsafe_to_string s1
  in
  let rec strip_heading_and_trailing_spaces s =
    if String.length s > 0 then
      if s.[0] = ' ' then
        strip_heading_and_trailing_spaces (String.sub s 1 (String.length s - 1))
      else if s.[String.length s - 1] = ' ' then
        strip_heading_and_trailing_spaces (String.sub s 0 (String.length s - 1))
      else s
    else s
  in
  if need_decode 0 then
    let len = compute_len 0 0 in
    let s1 = Bytes.create len in
    let s = copy_decode_in s1 0 0 in
    if strip_spaces then strip_heading_and_trailing_spaces s else s
  else s

let decode : Adef.encoded_string -> string = gen_decode true

let rec extract_param name stop_char =
  let case_unsensitive_eq s1 s2 =
    String.lowercase_ascii s1 = String.lowercase_ascii s2
  in
  function
  | x :: l ->
      if
        String.length x >= String.length name
        && case_unsensitive_eq (String.sub x 0 (String.length name)) name
      then
        let i =
          match String.index_from_opt x (String.length name) stop_char with
          | Some i -> i
          | None -> String.length x
        in
        String.sub x (String.length name) (i - String.length name)
      else extract_param name stop_char l
  | [] -> ""

let sprintf_date tm =
  Adef.safe
  @@ Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (1900 + tm.Unix.tm_year)
       (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
       tm.Unix.tm_sec

let rev_input_line ic pos (rbuff, rpos) =
  let rev = Buffer.create 256 in
  let rev_input_char pos =
    if !rpos = 0 then (
      if Bytes.length !rbuff < 65536 then
        rbuff :=
          Bytes.create
          @@ if Bytes.length !rbuff = 0 then 1024 else 2 * Bytes.length !rbuff;

      let ppos = max (pos - Bytes.length !rbuff) 0 in
      seek_in ic ppos;
      let len = pos - ppos in
      really_input ic !rbuff 0 len;
      rpos := len);
    decr rpos;
    Bytes.unsafe_get !rbuff !rpos
  in
  (* reverse buffer *)
  let get_n_reset () =
    let s = Buffer.to_bytes rev in
    let n = Bytes.length s in
    for i = 0 to (n - 1) / 2 do
      let c = Bytes.unsafe_get s i in
      Bytes.unsafe_set s i @@ Bytes.unsafe_get s (n - i - 1);
      Bytes.unsafe_set s (n - i - 1) c
    done;
    Bytes.unsafe_to_string s
  in
  let rev_input_line pos =
    if pos <= 0 then raise End_of_file
    else
      let _ = seek_in ic pos in
      let rec loop pos =
        if pos <= 0 then (get_n_reset (), pos)
        else
          match rev_input_char pos with
          | '\n' -> (get_n_reset (), pos)
          | '\r' -> (get_n_reset (), pos - 1)
          | c ->
              Buffer.add_char rev c;
              loop (pos - 1)
      in
      loop pos
  in
  rev_input_line (pos - 1)

let search_file_opt directories fname =
  let rec loop = function
    | hd :: tl ->
        let f = Filename.concat hd fname in
        if Sys.file_exists f then Some f else loop tl
    | [] -> None
  in
  loop directories

let search_asset_opt fname = search_file_opt (Secure.assets ()) fname

let eq_key (fn1, sn1, oc1) (fn2, sn2, oc2) =
  let s x = x |> nominative |> Name.lower in
  s fn1 = s fn2 && s sn1 = s sn2 && oc1 = oc2

let ls_r dirs =
  let rec loop acc = function
    | [] -> List.rev acc
    | dir :: rest ->
        if Sys.file_exists dir then
          if Sys.is_directory dir then
            let contents =
              try
                Sys.readdir dir |> Array.to_list
                |> List.map (Filename.concat dir)
              with e ->
                Printf.eprintf "Error reading directory %s: %s\n" dir
                  (Printexc.to_string e);
                []
            in
            loop (dir :: acc) (contents @ rest)
          else loop (dir :: acc) rest
        else (
          Printf.eprintf "Path does not exist: %s\n" dir;
          loop acc rest)
  in
  loop [] dirs

let check_permissions path =
  try
    let stats = Unix.stat path in
    let perms = stats.Unix.st_perm in
    if perms land 0o200 = 0 then
      Printf.eprintf "File is not writable: %s\n" path;
    try Unix.access path [ Unix.W_OK ]
    with Unix.Unix_error _ ->
      Printf.eprintf "Current process does not have write permission: %s\n" path
  with e ->
    Printf.eprintf "Error checking permissions for %s: %s\n" path
      (Printexc.to_string e)

let rm_rf f =
  if Sys.file_exists f then (
    let all_paths = ls_r [ f ] in
    List.iter check_permissions all_paths;
    let directories, files = List.partition Sys.is_directory all_paths in
    List.iter
      (fun file ->
        try
          let ic = open_in_bin file in
          close_in ic;
          Sys.remove file
        with e ->
          Printf.eprintf "Error handling file %s: %s\n" file
            (Printexc.to_string e))
      files;
    List.iter
      (fun dir ->
        try Unix.rmdir dir
        with e ->
          Printf.eprintf "Error deleting directory %s: %s\n" dir
            (Printexc.to_string e))
      (List.rev directories))
  else Printf.eprintf "Path does not exist: %s\n" f

let rec filter_map fn = function
  | [] -> []
  | hd :: tl -> (
      match fn hd with
      | Some x -> x :: filter_map fn tl
      | None -> filter_map fn tl)

let rec rev_iter fn = function
  | [] -> ()
  | hd :: tl ->
      let () = rev_iter fn tl in
      fn hd

let groupby ~key ~value list =
  let h = Hashtbl.create (List.length list) in
  List.iter
    (fun x ->
      let k = key x in
      let v = value x in
      if Hashtbl.mem h k then Hashtbl.replace h k (v :: Hashtbl.find h k)
      else Hashtbl.add h k [ v ])
    list;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []

let digest s = Digest.string s |> Digest.to_hex

let empty_person empty what =
  {
    Def.first_name = what;
    surname = what;
    occ = 0;
    public_name = empty;
    image = empty;
    qualifiers = [];
    aliases = [];
    first_names_aliases = [];
    surnames_aliases = [];
    titles = [];
    rparents = [];
    related = [];
    occupation = empty;
    sex = Neuter;
    access = IfTitles;
    birth = Date.cdate_None;
    birth_place = empty;
    birth_note = empty;
    birth_src = empty;
    baptism = Date.cdate_None;
    baptism_place = empty;
    baptism_note = empty;
    baptism_src = empty;
    death = DontKnowIfDead;
    death_place = empty;
    death_note = empty;
    death_src = empty;
    burial = UnknownBurial;
    burial_place = empty;
    burial_note = empty;
    burial_src = empty;
    pevents = [];
    notes = empty;
    psources = empty;
    key_index = ();
  }

let empty_family empty =
  {
    Def.marriage = Date.cdate_None;
    marriage_place = empty;
    marriage_note = empty;
    marriage_src = empty;
    witnesses = [||];
    relation = Def.NoMention;
    divorce = Def.NotDivorced;
    fevents = [];
    comment = empty;
    origin_file = empty;
    fsources = empty;
    fam_index = ();
  }

let good_name s =
  let rec loop i =
    if i = String.length s then true
    else
      match s.[i] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> loop (i + 1)
      | _ -> false
  in
  loop 0

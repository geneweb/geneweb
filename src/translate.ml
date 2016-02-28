(* camlp5r *)
(* $Id: translate.ml,v 5.9 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

module Buff = Buff.Make (struct end);

value skip_lang s =
  loop where rec loop i =
    if i = String.length s then None
    else
      match s.[i] with
      [ 'a'..'z' | '-' -> loop (i + 1)
      | _ -> Some i ]
;

value inline lang macro_char macro s =
  let lang = lang ^ ":" in
  let derived_lang =
    try
      let i = String.index lang '-' in
      String.sub lang 0 i ^ ":"
    with
    [ Not_found -> "" ]
  in
  let rec loop alt_version bol i =
    if i = String.length s then
      match alt_version with
      [ Some s -> (s, True)
      | None -> ("..........", False) ]
    else if bol then
      match skip_lang s i with
      [ Some j when s.[j] = ':' ->
          let curr_lang = String.sub s i (j + 1 - i) in
          if curr_lang = lang || curr_lang = derived_lang ||
             curr_lang = "en:" then
            let (s, i) =
              let j = if s.[j + 1] = ' ' then j + 1 else j in
              let rec loop len j =
                if j = String.length s then (Buff.get len, j)
                else if s.[j] = '\n' then
                  if j + 1 < String.length s && s.[j + 1] = ' ' then
                    let j =
                      loop (j + 1) where rec loop j =
                        if j < String.length s && s.[j] = ' ' then
                          loop (j + 1)
                        else j
                    in
                    loop (Buff.store len '\n') j
                  else (Buff.get len, j)
                else if s.[j] = macro_char then
                  loop (Buff.mstore len (macro s.[j + 1])) (j + 2)
                else loop (Buff.store len s.[j]) (j + 1)
              in
              loop 0 (j + 1)
            in
            if curr_lang = lang then (s, False)
            else
              let alt_version =
                if curr_lang = derived_lang then Some s
                else if alt_version = None then Some s
                else alt_version
              in
              loop alt_version True i
          else loop alt_version (s.[i] = '\n') (i + 1)
      | _ -> loop alt_version (s.[i] = '\n') (i + 1) ]
    else loop alt_version (s.[i] = '\n') (i + 1)
  in
  loop None True 0
;

value language_name lang lang_def =
  let str = lang_def in
  let len = String.length lang in
  let rec loop beg i =
    if i = String.length str && i = beg then lang
    else if i = String.length str || str.[i] = '/' then
      if i > beg + len + 1 && str.[beg + len] = '=' &&
         String.sub str beg len = lang then
        String.sub str (beg + len + 1) (i - beg - len - 1)
      else if i = String.length str then lang
      else loop (i + 1) (i + 1)
    else loop beg (i + 1)
  in
  loop 0 0
;

(* eval *)

value erase str i j =
  String.sub str 0 i ^ String.sub str j (String.length str - j)
;

(*
 * eval_set scans strings of the form @(x) where x is a list of characters
 * meaning a predicate to set for each character. Fills [set], the set of
 * predicates. Treats also the special case for @(&) = delete the next
 * character if any.
 *)

value eval_set str =
  loop [] str 0 where rec loop set str i =
    if i + 3 < String.length str then
      if str.[i] = '@' && str.[i + 1] = '(' && str.[i + 3] <> '?' &&
         str.[i + 3] <> '-'
      then
        if str.[i + 2] = '&' && str.[i + 3] = ')' && i + 4 < String.length str
        then
          loop set (erase str i (i + 5)) i
        else
          let (set, j) =
            loop set (i + 2) where rec loop set i =
              if i < String.length str then
                if str.[i] <> ')' then loop [str.[i] :: set] (i + 1)
                else (set, i + 1)
              else (set, i)
          in
          loop set (erase str i j) i
      else loop set str (i + 1)
    else (set, str)
;

value rec apply_expr set str i =
  if i + 1 < String.length str && str.[i + 1] = '?' then
    if List.mem str.[i] set then
      let str = erase str i (i + 2) in
      let (str, i) = apply_expr set str i in
      if i < String.length str && str.[i] = ':' then
        let (str, j) = apply_expr set str (i + 1) in
        (erase str i j, i)
      else (str, i)
    else
      let (str, j) = apply_expr set str (i + 2) in
      let str = erase str i j in
      if i < String.length str && str.[i] = ':' then
        let str = erase str i (i + 1) in
        apply_expr set str i
      else (str, i)
  else if i < String.length str && (str.[i] = ':' || str.[i] = ')') then
    (str, i)
  else apply_expr set str (i + 1)
;

(*
 * eval_app scans strings matching expressions between @( and ).
 *    an expression is:
 *     - a [character] followed by "?", an [expression] and possibly ":" and
 *       [another expression]
 *     - any [string] not holding ":"
 *    The [character] is a predicate. If defined, the first expression is
 *    evaluated, else it is the second one. The evaluation of a string is
 *    itself.
 *
 *  ex: p?e:m?A?en:er:w?e:n?es
 *    In this example, if m and A are only defined predicates:
 *      p not being defined, it is m?A?en:er:w?e:n?es
 *      m being defined, it is A?en:er
 *      A being defined, it is en
 *    This example shows how to display adjectives in German, where
 *    m is for masculine, w for feminine and n for neuter
 *)

value eval_app set str =
  loop str 0 where rec loop str i =
    if i + 3 < String.length str then
      if str.[i] = '@' && str.[i + 1] = '(' && str.[i + 3] <> '-' then
        let str = erase str i (i + 2) in
        let (str, i) = apply_expr set str i in
        if i < String.length str then
          if str.[i] = ')' then loop (erase str i (i + 1)) i else loop str i
        else str
      else loop str (i + 1)
    else str
;

(*
 * eval_shift scans strings matching:
 *   @(#-) shifting # words of the left after the next word.
 *   @(#--) shifting # words of the left to the end.
 * ex:
 *   before: "Une avec un diamant@(3-) bague"
 *    after: "Une bague avec un diamant"
 *   before: "Sie haben geworfen@(1--) einen kurzen Bogen"
 *    after: "Sie haben einen kurzen Bogen geworfen"
 *)

value rec eval_shift s =
  let t = Bytes.make (String.length s) '#' in
  let rec loop changed i j =
    if i + 4 < String.length s && s.[i] = '@' && s.[i + 1] = '(' &&
       s.[i + 3] = '-'
    then
      let nleft = Char.code s.[i + 2] - Char.code '0' in
      let to_the_end = s.[i + 4] = '-' in
      let k = if to_the_end then i + 5 else i + 4 in
      if k < String.length s && s.[k] = ')' then
        let l =
          loop nleft (i - 1) where rec loop nleft l =
            if l > 0 then
              if s.[l] = ' ' then
                if nleft <= 1 then l + 1 else loop (nleft - 1) (l - 1)
              else loop nleft (l - 1)
            else 0
        in
        let len = i - l in
        let j = j - len in
        let k = k + 1 in
        let i = if k < String.length s && s.[k] = ' ' then k + 1 else k in
        let (i, j) =
          if to_the_end then
            let rec loop i j =
              if i < String.length s then do {
                Bytes.set t j s.[i]; loop (i + 1) (j + 1)
              }
              else do {
                let j =
                  if j > 0 && (Bytes.get t (j - 1)) <> ' ' then do { Bytes.set t j ' '; j + 1 }
                  else j
                in
                String.blit s l t j len;
                (i, j + len)
              }
            in
            loop i j
          else
            let rec loop i j =
              if i < String.length s then
                if s.[i] = ' ' then do {
                  Bytes.set t j ' ';
                  String.blit s l t (j + 1) len;
                  (i, j + 1 + len)
                }
                else do { Bytes.set t j s.[i]; loop (i + 1) (j + 1) }
              else if k < String.length s && s.[k] = ' ' then do {
                Bytes.set t j ' '; String.blit s l t (j + 1) len; (i, j + 1 + len)
              }
              else do { String.blit s l t j len; (i, j + len) }
            in
            loop i j
        in
        loop True i j
      else do { Bytes.set t j s.[i]; loop changed (i + 1) (j + 1) }
    else if i < String.length s then do {
      Bytes.set t j s.[i]; loop changed (i + 1) (j + 1)
    }
    else if changed then eval_shift (Bytes.sub_string t 0 j)
    else Bytes.sub_string t 0 j
  in
  loop False 0 0
;

value rec eval str =
  if not (String.contains str '@') then (* optimisation *) str
  else
    let str = eval_rec str in
    let (set, str) = eval_set str in
    let str = eval_app set str in
    eval_shift str
and eval_rec str =
  loop str 0 where rec loop str i =
    if i = String.length str then str
    else if i + 3 < String.length str && str.[i] = '@' && str.[i+1] = '(' &&
      str.[i+2] = '@'
    then
      let j =
        loop (i + 2) where rec loop j =
          if j < String.length str then
            if str.[j] = '(' then
              let j = loop (j + 1) in
              loop (j + 1)
            else if str.[j] = ')' then j
            else loop (j + 1)
          else j
      in
      if j = String.length str then str
      else
        let sstr = eval (String.sub str (i + 2) (j - i - 2)) in
        let k = i + String.length sstr in
        let str =
          String.sub str 0 i ^ sstr ^
            String.sub str (j + 1) (String.length str - j - 1)
        in
        loop str k
    else loop str (i + 1)
;

(* camlp4r *)
(* $Id: translate.ml,v 5.0 2005-12-13 11:51:27 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

module Buff = Buff.Make (struct value buff = ref (String.create 80); end);

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
                else if s.[j] == macro_char then
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
    if i == String.length str && i == beg then lang
    else if i == String.length str || str.[i] == '/' then
      if i > beg + len + 1 && str.[beg + len] = '=' &&
         String.sub str beg len = lang then
        String.sub str (beg + len + 1) (i - beg - len - 1)
      else if i == String.length str then lang
      else loop (i + 1) (i + 1)
    else loop beg (i + 1)
  in
  loop 0 0
;

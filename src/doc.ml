(* $Id: doc.ml,v 4.1 2001-04-22 14:37:59 ddr Exp $ *)

open Config;

value start_with s i p =
  i + String.length p < String.length s &&
  String.lowercase (String.sub s i (String.length p)) = p
;

value last_is s i p =
  loop i (String.length p - 1) where rec loop i k =
    if i <= 0 then False
    else if k < 0 then True
    else
      let c = Char.lowercase s.[i] in
      let c = if c = '\n' || c = '\r' then ' ' else c in
      if c = ' ' && p.[k] = ' ' then
        let rec loop1 i =
          if i <= 0 then False
          else
            match s.[i] with
            [ '\n' | '\r' | ' ' -> loop1 (i - 1)
            | _ -> loop i (k - 1) ]
        in
        loop1 (i - 1)
      else if c = p.[k] then loop (i - 1) (k - 1)
      else False
;

value http = "http://";

value url_basename name =
  try
    let p = String.rindex name '/' + 1 in
    String.sub name p (String.length name - p)
  with
  [ Not_found -> name ]
;

value url_dirname name =
  try
    match String.rindex name '/' with
    [ 0 -> "/"
    | n -> String.sub name 0 n ]
  with
  [ Not_found -> "." ]
;

value url_is_relative n = String.length n < 1 || n.[0] <> '/';

value url_is_implicit n =
  url_is_relative n && (String.length n < 2 || String.sub n 0 2 <> "./") &&
  (String.length n < 3 || String.sub n 0 3 <> "../")
;

value copy conf pref_doc pref_img s =
  loop 0 where rec loop i =
    if i == String.length s then ()
    else if last_is s i "<a href=" then do {
      let i = do { Wserver.wprint "="; i + 1 } in
      let i = if s.[i] = '"' then do { Wserver.wprint "\""; i + 1 } else i in
      if s.[i] = '#' || start_with s i http || start_with s i "mailto:" then
        ()
      else Wserver.wprint "%s" pref_doc;
      loop i
    }
    else if last_is s i " src=" || last_is s i " background=" then do {
      let i = do { Wserver.wprint "="; i + 1 } in
      let (img, i) =
        if s.[i] = '"' then
          let rec loop i len =
            if i = String.length s then (Buff.get len, i)
            else if s.[i] = '"' then (Buff.get len, i + 1)
            else loop (i + 1) (Buff.store len s.[i])
          in
          loop (i + 1) 0
        else
          let rec loop i len =
            if i = String.length s then (Buff.get len, i)
            else if s.[i] = '>' then (Buff.get len, i)
            else loop (i + 1) (Buff.store len s.[i])
          in
          loop (i + 1) 0
      in
      let img = url_basename img in
      Wserver.wprint "\"%s%s\"" pref_img img;
      loop i
    }
    else if last_is s i "<body>" then do {
      Wserver.wprint "%s>" (Util.body_prop conf); loop (i + 1)
    }
    else do { Wserver.wprint "%c" s.[i]; loop (i + 1) }
;

value mac_name_of_url_name s =
  loop 0 0 where rec loop i len =
    if i == String.length s then Buff.get len
    else if s.[i] == '/' then loop (Buff.store len ':') (i + 1)
    else loop (Buff.store len s.[i]) (i + 1)
;

value print conf =
  let v =
    match Util.p_getenv conf.env "v" with
    [ Some f -> f
    | None -> "" ]
  in
  let v = if v = "" then "index.htm" else v in
  if url_is_implicit v then
    let fname = if Sys.os_type = "MacOS" then mac_name_of_url_name v else v in
    let fname =
      if Filename.check_suffix fname ".htm" then fname else fname ^ ".htm"
    in
    let fname = Filename.concat Util.doc_dir.val fname in
    match try Some (open_in fname) with [ Sys_error _ -> None ] with
    [ Some ic ->
        do {
          Util.html conf;
          let s =
            let len = ref 0 in
            do {
              try
                let rec loop () =
                  do {
                    len.val := Buff.store len.val (input_char ic); loop ()
                  }
                in
                loop ()
              with
              [ End_of_file -> close_in ic ];
              Buff.get len.val
            }
          in
          let pref_doc =
            let dir = url_dirname v ^ "/" in
            let dir = if dir = "./" then "" else dir in
            conf.indep_command ^ "m=DOC;v=" ^ dir
          in
          let pref_img = conf.indep_command ^ "m=IM;v=/" in
          copy conf pref_doc pref_img s
        }
    | None -> Util.incorrect_request conf ]
  else Util.incorrect_request conf
;

open Def.NLDB
module Driver = Geneweb_db.Driver

let char_dir_sep = ':'
let dir_sep = ":"

let check_file_name s =
  let rec loop path ibeg i =
    if i = String.length s then
      if i > ibeg then
        let path = (List.rev path, String.sub s ibeg (i - ibeg)) in
        Some path
      else None
    else
      match s.[i] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' ->
          loop path ibeg (i + 1)
      | c ->
          if c = char_dir_sep then
            if i > ibeg then
              loop (String.sub s ibeg (i - ibeg) :: path) (i + 1) (i + 1)
            else None
          else None
  in
  loop [] 0 0

type wiki_link =
  | WLpage of int * (string list * string) * string * string * string
  | WLperson of int * key * string option * string option
  | WLwizard of int * string * string
  | WLnone of int * string

(* search in a note (s) wiki references
   to persons [[fn/sn/oc/txt]] or pages [[[page/text]]] *)
let misc_notes_link s i =
  let slen = String.length s in
  let cut j = String.sub s i (j - i) in
  let rec wlnone j =
    (* assume no link up to [j] and find next link position *)
    if j < slen then
      match s.[j] with
      | '%' -> WLnone (j, cut j)
      | '\'' -> WLnone (j, cut j)
      | '{' -> WLnone (j, cut j)
      | '[' ->
          if j > i && j + 1 < slen && s.[j + 1] = '[' then WLnone (j, cut j)
          else wlnone (j + 1)
      | _ -> wlnone (j + 1)
    else WLnone (slen, cut slen)
  in
  if i < slen - 2 && s.[i] = '[' && s.[i + 1] = '[' then
    if s.[i + 2] = '[' then
      let j =
        let rec loop j =
          if j = slen then i
          else if
            j <= slen - 3 && s.[j] = ']' && s.[j + 1] = ']' && s.[j + 2] = ']'
          then j + 3
          else loop (j + 1)
        in
        loop (i + 3)
      in
      if j > i + 6 then
        let b = String.sub s (i + 3) (j - i - 6) in
        (* anchor does not contains the leading '#' *)
        let fname, anchor, text =
          match String.rindex_opt b '/' with
          | None -> (b, "", b)
          | Some k ->
              (* anchor_start = k implies empty anchor *)
              let fname_len, anchor_start =
                match String.rindex_from_opt b k '#' with
                | None -> (k, k)
                | Some j -> (j, min (j + 1) k)
              in
              ( String.sub b 0 fname_len,
                String.sub b anchor_start (k - anchor_start),
                String.sub b (k + 1) (String.length b - k - 1) )
        in
        match check_file_name fname with
        | Some pg_path -> WLpage (j, pg_path, fname, anchor, text)
        | None -> wlnone j
      else wlnone j
    else
      let j =
        let rec loop j =
          if j = slen then j
          else if j <= slen - 2 && s.[j] = ']' && s.[j + 1] = ']' then j + 2
          else loop (j + 1)
        in
        loop (i + 2)
      in
      if j > i + 4 then
        let b = String.sub s (i + 2) (j - i - 4) in
        let spe, b =
          match String.index_opt b ':' with
          | Some i ->
              ( Some (String.sub b 0 i),
                String.sub b (i + 1) (String.length b - i - 1) )
          | None -> (None, b)
        in
        let b, text =
          match String.index_opt b ';' with
          | Some i ->
              ( String.sub b 0 i,
                Some (String.sub b (i + 1) (String.length b - i - 1)) )
          | None -> (b, None)
        in
        if spe = Some "w" then
          let wiz, name =
            match String.index_opt b '/' with
            | Some i ->
                ( String.sub b 0 i,
                  String.sub b (i + 1) (String.length b - i - 1) )
            | None -> (b, "")
          in
          WLwizard (j, wiz, name)
        else
          try
            let k = 0 in
            let l = String.index_from b k '/' in
            let fn = String.sub b k (l - k) in
            let k = l + 1 in
            let fn, sn, oc, name =
              try
                let l = String.index_from b k '/' in
                let sn = String.sub b k (l - k) in
                let oc, name =
                  try
                    let k = l + 1 in
                    let l = String.index_from b k '/' in
                    let x = String.sub b k (l - k) in
                    (x, String.sub b (l + 1) (String.length b - l - 1))
                  with Not_found ->
                    ("", String.sub b (l + 1) (String.length b - l - 1))
                in
                let oc1 = try int_of_string name with Failure _ -> -1 in
                let oc = try int_of_string oc with Failure _ -> 0 in
                if oc1 = -1 then (fn, sn, oc, Some name)
                else (fn, sn, oc1, Some (fn ^ " " ^ sn))
              with Not_found ->
                let sn = String.sub b k (String.length b - k) in
                (fn, sn, 0, Some (fn ^ " " ^ sn))
            in
            let fn = Name.lower fn in
            let sn = Name.lower sn in
            WLperson (j, (fn, sn, oc), name, text)
          with Not_found -> wlnone j
      else wlnone j
  else wlnone (i + 1)

let add_in_db db who (list_nt, list_ind) =
  let db = List.remove_assoc who db in
  if list_nt = [] && list_ind = [] then db else (who, (list_nt, list_ind)) :: db

let update_db base who list =
  Driver.write_nldb base @@ add_in_db (Driver.read_nldb base) who list

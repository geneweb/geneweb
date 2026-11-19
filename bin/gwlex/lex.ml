type line = { lang : string; text : string; lnum : int }
type entry = { key : string; key_lnum : int; lines : line list }

type t = {
  entries : entry list;
  errors : (int * string) list;
  crlf : bool;
  source : string;
}

let is_blank s = String.for_all (fun c -> c = ' ' || c = '\t') s

let is_lang s =
  s = "->"
  || s <> ""
     && s.[0] <> '-'
     && s.[String.length s - 1] <> '-'
     && String.for_all (fun c -> (c >= 'a' && c <= 'z') || c = '-') s

let parse_line lnum s =
  match String.index_opt s ':' with
  | Some i when is_lang (String.sub s 0 i) ->
      let lang = String.sub s 0 i in
      let len = String.length s in
      if i + 1 = len then Ok { lang; text = ""; lnum }
      else if s.[i + 1] = ' ' then
        Ok { lang; text = String.sub s (i + 2) (len - i - 2); lnum }
      else Error "missing space after the colon"
  | Some _ | None -> Error "malformed translation line"

let read fname =
  let source = In_channel.with_open_bin fname In_channel.input_all in
  let crlf =
    match String.index_opt source '\n' with
    | Some i -> i > 0 && source.[i - 1] = '\r'
    | None -> false
  in
  let chop s =
    let len = String.length s in
    if len > 0 && s.[len - 1] = '\r' then String.sub s 0 (len - 1) else s
  in
  let close cur entries =
    match cur with
    | Some e -> { e with lines = List.rev e.lines } :: entries
    | None -> entries
  in
  let rec loop lnum cur entries errors = function
    | [] ->
        {
          entries = List.rev (close cur entries);
          errors = List.rev errors;
          crlf;
          source;
        }
    | s :: rest -> (
        let s = chop s in
        if is_blank s then loop (lnum + 1) None (close cur entries) errors rest
        else if String.starts_with ~prefix:"    " s then
          let key = String.sub s 4 (String.length s - 4) in
          let e = { key; key_lnum = lnum; lines = [] } in
          loop (lnum + 1) (Some e) (close cur entries) errors rest
        else
          match cur with
          | None ->
              let errors = (lnum, "line outside an entry") :: errors in
              loop (lnum + 1) None entries errors rest
          | Some e -> (
              match parse_line lnum s with
              | Ok l ->
                  let cur = Some { e with lines = l :: e.lines } in
                  loop (lnum + 1) cur entries errors rest
              | Error msg ->
                  loop (lnum + 1) cur entries ((lnum, msg) :: errors) rest))
  in
  loop 1 None [] [] (String.split_on_char '\n' source)

let compare_key k1 k2 =
  match
    String.compare (String.lowercase_ascii k1) (String.lowercase_ascii k2)
  with
  | 0 -> String.compare k1 k2
  | c -> c

let sort entries =
  entries
  |> List.map (fun e ->
      let by_lang l1 l2 = String.compare l1.lang l2.lang in
      { e with lines = List.stable_sort by_lang e.lines })
  |> List.stable_sort (fun e1 e2 -> compare_key e1.key e2.key)

let add_line b nl lang text =
  if text = "" then Printf.bprintf b "%s:%s" lang nl
  else Printf.bprintf b "%s: %s%s" lang text nl

let to_string ~crlf entries =
  let nl = if crlf then "\r\n" else "\n" in
  let b = Buffer.create 0x200000 in
  List.iteri
    (fun i e ->
      if i > 0 then Buffer.add_string b nl;
      Printf.bprintf b "    %s%s" e.key nl;
      List.iter (fun l -> add_line b nl l.lang l.text) e.lines)
    entries;
  Buffer.contents b

let duplicates entries =
  let keys = Hashtbl.create 4096 in
  List.fold_left
    (fun acc e ->
      let acc =
        match Hashtbl.find_opt keys e.key with
        | Some n ->
            let msg = Printf.sprintf "duplicate key [%s], first at line %d" in
            (e.key_lnum, msg e.key n) :: acc
        | None ->
            Hashtbl.replace keys e.key e.key_lnum;
            acc
      in
      let langs = Hashtbl.create 64 in
      List.fold_left
        (fun acc l ->
          match Hashtbl.find_opt langs l.lang with
          | Some n ->
              let msg =
                Printf.sprintf "duplicate language %s in [%s], first at line %d"
              in
              (l.lnum, msg l.lang e.key n) :: acc
          | None ->
              Hashtbl.replace langs l.lang l.lnum;
              acc)
        acc e.lines)
    [] entries
  |> List.rev

let fields s =
  let len = String.length s in
  let rec loop acc start i =
    if i >= len then List.rev (String.sub s start (len - start) :: acc)
    else
      match s.[i] with
      | '<' -> loop acc start (i + 2)
      | '/' -> loop (String.sub s start (i - start) :: acc) (i + 1) (i + 1)
      | _ -> loop acc start (i + 1)
  in
  loop [] 0 0

let signature s =
  let len = String.length s in
  let rec loop convs args i =
    if i + 1 >= len then (List.rev convs, List.sort_uniq Char.compare args)
    else if s.[i] <> '%' then loop convs args (i + 1)
    else
      match s.[i + 1] with
      | ('s' | 'd' | 't') as c -> loop (c :: convs) args (i + 2)
      | '1' .. '9' as c -> loop convs (c :: args) (i + 2)
      | _ -> loop convs args (i + 2)
  in
  loop [] [] 0

let format_errors entries =
  List.concat_map
    (fun e ->
      match List.find_opt (fun l -> l.lang = "en") e.lines with
      | None -> []
      | Some en ->
          let whole = signature en.text in
          let variants = List.map signature (fields en.text) in
          let matches l =
            signature l.text = whole
            || List.for_all
                 (fun v -> List.mem (signature v) variants)
                 (fields l.text)
          in
          List.filter_map
            (fun l ->
              if l.lang = "en" || l.lang = "->" || matches l then None
              else
                let msg =
                  Printf.sprintf "[%s] %s: placeholders differ from en"
                in
                Some (l.lnum, msg e.key l.lang))
            e.lines)
    entries

let languages entries =
  entries
  |> List.concat_map (fun e ->
      List.filter_map
        (fun l -> if l.lang = "->" then None else Some l.lang)
        e.lines)
  |> List.sort_uniq String.compare

let missing entries langs =
  let b = Buffer.create 0x10000 in
  List.iter
    (fun e ->
      let find lang = List.find_opt (fun l -> l.lang = lang) e.lines in
      let variants l = List.length (fields l.text) in
      let en = find "en" in
      let todo =
        List.filter_map
          (fun lang ->
            match (find lang, en) with
            | None, _ -> Some (lang, "")
            | Some l, Some r when variants l <> variants r -> Some (lang, l.text)
            | Some _, _ -> None)
          langs
      in
      if find "->" = None && todo <> [] then (
        Printf.bprintf b "    %s\n" e.key;
        List.iter
          (fun lang ->
            match find lang with
            | Some l when not (List.mem_assoc lang todo) ->
                add_line b "\n" lang l.text
            | Some _ | None -> ())
          [ "en"; "fr" ];
        List.iter (fun (lang, text) -> add_line b "\n" lang text) todo;
        Buffer.add_char b '\n'))
    entries;
  Buffer.contents b

(* Copyright (c) 1998-2007 INRIA *)

let limit_display_length =
  let max_display_length = 4 * 1_000_000 in
  fun s -> String.sub s 0 (min (String.length s) max_display_length)

let file_path conf base fname =
  GWPARAM.bpath
    (List.fold_left Filename.concat
       (conf.Config.bname ^ ".gwb")
       [ Gwdb.base_notes_dir base; fname ^ ".txt" ])

let path_of_fnotes fnotes =
  match NotesLinks.check_file_name fnotes with
  | Some (dl, f) -> List.fold_right Filename.concat dl f
  | None -> ""

let read_notes ?(limit = true) base fnotes =
  let fnotes = path_of_fnotes fnotes in
  let s =
    (if limit then limit_display_length else Fun.id)
    @@ Gwdb.base_notes_read base fnotes
  in
  Wiki.split_title_and_text s

let merge_possible_aliases conf db =
  let aliases = Wiki.notes_aliases conf in
  let db =
    List.map
      (fun (pg, (sl, il)) ->
        let pg =
          match pg with
          | Def.NLDB.PgMisc f -> Def.NLDB.PgMisc (Wiki.map_notes aliases f)
          | x -> x
        in
        let sl = List.map (Wiki.map_notes aliases) sl in
        (pg, (sl, il)))
      db
  in
  let db = List.sort (fun (pg1, _) (pg2, _) -> compare pg1 pg2) db in
  List.fold_left
    (fun list (pg, (sl, il)) ->
      let sl, _il1, list =
        let list1, list2 =
          match list with
          | ((pg1, _) as x) :: l -> if pg = pg1 then ([ x ], l) else ([], list)
          | [] -> ([], list)
        in
        match list1 with
        | [ (_, (sl1, il1)) ] ->
            let sl =
              List.fold_left
                (fun sl s -> if List.mem s sl then sl else s :: sl)
                sl sl1
            in
            let il =
              List.fold_left
                (fun il i -> if List.mem i il then il else i :: il)
                il il1
            in
            (sl, il, list2)
        | _ -> (sl, il, list)
      in
      (pg, (sl, il)) :: list)
    [] db

let notes_links_db conf base eliminate_unlinked =
  let db = Gwdb.read_nldb base in
  let db = merge_possible_aliases conf db in
  let db2 =
    List.fold_left
      (fun db2 (pg, (sl, _il)) ->
        let record_it =
          match pg with
          | Def.NLDB.PgInd ip ->
              Util.pget conf base ip |> Person.is_visible conf base
          | Def.NLDB.PgFam ifam ->
              Gwdb.foi base ifam |> Gwdb.get_father |> Util.pget conf base
              |> Person.is_visible conf base
          | Def.NLDB.PgNotes | Def.NLDB.PgMisc _ | Def.NLDB.PgWizard _ -> true
        in
        if record_it then
          List.fold_left
            (fun db2 s ->
              match List.assoc_opt s db2 with
              | Some list -> (s, pg :: list) :: List.remove_assoc s db2
              | None -> (s, [ pg ]) :: db2)
            db2 sl
        else db2)
      [] db
  in
  (* some kind of basic gc... *)
  let misc = Hashtbl.create 1 in
  let set =
    List.fold_left
      (fun set (pg, (sl, _il)) ->
        match pg with
        | Def.NLDB.PgInd _ | Def.NLDB.PgFam _ | Def.NLDB.PgNotes
        | Def.NLDB.PgWizard _ ->
            List.fold_left (fun set s -> Ext_string.Set.add s set) set sl
        | Def.NLDB.PgMisc s ->
            Hashtbl.add misc s sl;
            set)
      Ext_string.Set.empty db
  in
  let mark = Hashtbl.create 1 in
  (let rec loop = function
     | s :: sl ->
         if Hashtbl.mem mark s then loop sl
         else (
           Hashtbl.add mark s ();
           let sl1 = Option.value (Hashtbl.find_opt misc s) ~default:[] in
           loop (List.rev_append sl1 sl))
     | [] -> ()
   in
   loop (Ext_string.Set.elements set));
  let is_referenced s = Hashtbl.mem mark s in
  let db2 =
    if eliminate_unlinked then
      List.fold_right
        (fun (s, list) db2 -> if is_referenced s then (s, list) :: db2 else db2)
        db2 []
    else db2
  in
  List.sort
    (fun (s1, _) (s2, _) ->
      Utf8.alphabetic_order (Name.lower s1) (Name.lower s2))
    db2

let update_notes_links_db base fnotes s =
  let slen = String.length s in
  let list_nt, list_ind =
    let rec loop list_nt list_ind pos i =
      if i = slen then (list_nt, list_ind)
      else if i + 1 < slen && s.[i] = '%' then loop list_nt list_ind pos (i + 2)
      else
        match NotesLinks.misc_notes_link s i with
        | NotesLinks.WLpage (j, _, lfname, _, _) ->
            let list_nt =
              if List.mem lfname list_nt then list_nt else lfname :: list_nt
            in
            loop list_nt list_ind pos j
        | NotesLinks.WLperson (j, key, _, txt) ->
            let list_ind =
              let link = { Def.NLDB.lnTxt = txt; Def.NLDB.lnPos = pos } in
              (key, link) :: list_ind
            in
            loop list_nt list_ind (pos + 1) j
        | NotesLinks.WLwizard (j, _, _) -> loop list_nt list_ind pos j
        | NotesLinks.WLnone -> loop list_nt list_ind pos (i + 1)
    in
    loop [] [] 1 0
  in
  NotesLinks.update_db base fnotes (list_nt, list_ind)

let commit_notes conf base fnotes s =
  let pg = if fnotes = "" then Def.NLDB.PgNotes else Def.NLDB.PgMisc fnotes in
  let fname = path_of_fnotes fnotes in
  let fpath =
    List.fold_left Filename.concat
      (GWPARAM.bpath (conf.Config.bname ^ ".gwb"))
      [ Gwdb.base_notes_dir base; fname ]
  in
  Files.mkdir_p (Filename.dirname fpath);
  Gwdb.commit_notes base fname s;
  History.record conf base
    (Def.U_Notes (Util.p_getint conf.Config.env "v", fnotes))
    "mn";
  update_notes_links_db base pg s

let remove_empty_txt_head = function `Text [ "" ] :: xs -> xs | xs -> xs

let insert_brs_in_txt br_met ss =
  let rec insert wiki_context = function
    | [ x ] -> [ x ]
    | (`Text txt as x) :: [ `Text [ "" ] ] when txt <> [ "" ] ->
        x :: [ `Text [ "\n" ] ]
    | (`Text txt as x) :: `Text [ "" ] :: xs when txt <> [ "" ] ->
        x :: `Text [ "\n\n" ] :: insert false xs
    | (`Text [ txt ] as x) :: xs when Wiki.line_is_in_wiki_syntax txt ->
        x :: `Text [ "\n" ] :: insert true xs
    | x :: xs when wiki_context -> x :: `Text [ "\n" ] :: insert true xs
    | x :: xs ->
        x
        :: `Start_element ((Markup.Ns.html, "br"), [])
        :: `End_element :: `Text [ "\n" ] :: insert false xs
    | [] -> []
  in
  let lines : string list =
    List.flatten @@ List.map (String.split_on_char '\n') ss
  in
  let signals = List.map (fun s -> `Text [ s ]) lines in
  let signals' = if br_met then remove_empty_txt_head signals else signals in
  if List.exists (fun s -> s <> `Text [ "" ]) signals' then
    if br_met && List.hd signals = `Text [ "" ] then
      `Text [ "\n" ] :: insert false signals'
    else insert false signals'
  else [ `Text ss ]

let insert_brs s =
  let depth_stack = Stack.create () in
  let push_depth d = Stack.push d depth_stack in
  let pop_depth () = Option.value ~default:0 (Stack.pop_opt depth_stack) in
  let decrease_depth d = if d = 0 then d else d - 1 in
  let insert_brs_in_toplvl_text (br_met, depth) v =
    match v with
    | `Start_element ((_, "br"), _) -> ([ v ], Some (true, depth + 1))
    | `Start_element
        ((_, ("em" | "i" | "b" | "s" | "u" | "strong" | "strike")), _) ->
        push_depth depth;
        ([ v ], Some (false, 0))
    | `Start_element (_, _) -> ([ v ], Some (false, depth + 1))
    | `End_element when depth = 0 -> ([ v ], Some (br_met, pop_depth ()))
    | `End_element -> ([ v ], Some (br_met, decrease_depth depth))
    | `Text txts when depth = 0 ->
        let txt = insert_brs_in_txt br_met txts in
        (txt, Some (false, depth))
    | `Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _ ->
        ([ v ], Some (false, depth))
  in
  let signals =
    Markup.string s
    |> Markup.parse_html ~context:(`Fragment "body")
    |> Markup.signals
  in
  Markup.transform insert_brs_in_toplvl_text (false, 0) signals
  |> Markup.write_html |> Markup.to_string

let wiki_aux ?(keep_newlines = false) pp conf base env str =
  let s = Util.string_with_macros ~conf ~env (limit_display_length str) in
  let s = if keep_newlines then insert_brs s else s in
  let lines = pp (Wiki.html_of_tlsw conf s) in
  let wi =
    {
      Wiki.wi_mode = "NOTES";
      Wiki.wi_file_path = file_path conf base;
      Wiki.wi_person_exists = Util.person_exists conf base;
      Wiki.wi_always_show_link = conf.Config.wizard || conf.Config.friend;
    }
  in
  String.concat "\n" lines |> Wiki.syntax_links conf base wi |> Util.safe_html

let source conf base str =
  wiki_aux (function [ "<p>"; x; "</p>" ] -> [ x ] | x -> x) conf base [] str

let note ?(keep_newlines = false) conf base env str =
  wiki_aux ~keep_newlines Fun.id conf base env str

let person_note ?(keep_newlines = false) conf base p str =
  let env = [ ('i', fun () -> Image.default_portrait_filename base p) ] in
  note ~keep_newlines conf base env str

let source_note conf base p str =
  let env = [ ('i', fun () -> Image.default_portrait_filename base p) ] in
  wiki_aux (function [ "<p>"; x; "</p>" ] -> [ x ] | x -> x) conf base env str

let source_note_with_env ?(keep_newlines = false) conf base env str =
  wiki_aux ~keep_newlines
    (function [ "<p>"; x; "</p>" ] -> [ x ] | x -> x)
    conf base env str

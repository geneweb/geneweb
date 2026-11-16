open Config

let print conf base =
  let salt = Option.get conf.secret_salt in
  let digest =
    match Util.p_getenv conf.env "t" with
    | Some "note" ->
        let f = Option.value ~default:"" (Util.p_getenv conf.env "f") in
        if f = "" || NotesLinks.check_file_name f <> None then
          let env, s = Notes.read_notes base f in
          Some
            (Mutil.digest
               (List.fold_left (fun s (k, v) -> s ^ k ^ "=" ^ v ^ "\n") "" env
               ^ s))
        else None
    | Some "fam" ->
        Option.map
          (fun (ifam, _) ->
            Update.digest_family ~salt (UpdateFam.string_family_of base ifam))
          (Util.find_family_in_env conf base)
    | Some _ | None ->
        Option.map
          (fun p ->
            Update.digest_person ~salt (UpdateInd.string_person_of base p))
          (Util.find_person_in_env conf base "")
  in
  let changed = digest <> Util.p_getenv conf.env "digest" in
  Output.header conf "Content-type: application/json";
  Output.header conf "Connection: close";
  Output.print_sstring conf
    (Yojson.Basic.to_string (`Assoc [ ("changed", `Bool changed) ]));
  Output.flush conf

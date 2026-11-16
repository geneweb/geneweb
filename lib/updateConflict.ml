open Config
open Def
module Driver = Geneweb_db.Driver

let executable_stamp () =
  match Unix.stat Sys.executable_name with
  | st -> Printf.sprintf "%.0f/%d" st.Unix.st_mtime st.Unix.st_size
  | exception Unix.Unix_error _ -> ""

let mac conf kind payload =
  Digestif.SHA256.hmac_string
    ~key:(Option.value ~default:"" conf.secret_salt)
    (String.concat "\000"
       [ kind; Version.commit_id; executable_stamp (); payload ])

let seal conf kind v =
  let payload = Marshal.to_string v [] in
  Digestif.SHA256.to_hex (mac conf kind payload)
  ^ "."
  ^ Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet payload

let unseal conf kind s =
  match String.index_opt s '.' with
  | None -> None
  | Some i -> (
      let payload =
        Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet
          (String.sub s (i + 1) (String.length s - i - 1))
      in
      match (Digestif.SHA256.of_hex_opt (String.sub s 0 i), payload) with
      | Some m, Ok payload when Digestif.SHA256.equal m (mac conf kind payload)
        ->
          Some (Marshal.from_string payload 0)
      | Some _, Ok _ | Some _, Error _ | None, _ -> None)

let person_snapshot conf (sp : (Driver.iper, Update.key, string) gen_person) =
  seal conf "person" sp

let person_of_snapshot conf ip s =
  match
    (unseal conf "person" s
      : (Driver.iper, Update.key, string) gen_person option)
  with
  | Some sp when Driver.Iper.equal sp.key_index ip -> Some sp
  | Some _ | None -> None

let family_snapshot conf
    (fcd :
      (Update.key, Driver.ifam, string) gen_family
      * Update.key Adef.gen_couple
      * Update.key gen_descend) =
  seal conf "family" fcd

let family_of_snapshot conf ifam s =
  match
    (unseal conf "family" s
      : ((Update.key, Driver.ifam, string) gen_family
        * Update.key Adef.gen_couple
        * Update.key gen_descend)
        option)
  with
  | Some ((sfam, _, _) as fcd) when Driver.Ifam.equal sfam.fam_index ifam ->
      Some fcd
  | Some _ | None -> None

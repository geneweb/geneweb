open Geneweb
open Config
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let ns = "export"

module IPS = Set.Make (struct
  type t = Driver.iper

  let compare = compare
end)

module IFS = Set.Make (struct
  type t = Driver.ifam

  let compare = compare
end)

let w_lock =
  Gwd_lib.Request.w_lock ~onerror:(fun conf _ -> Update.error_locked conf)

let w_base =
  Gwd_lib.Request.w_base ~none:(fun conf ->
      Hutil.incorrect_request conf;
      true)

let getenv var env = List.assoc var env |> Mutil.decode
let getenv_opt var env = List.assoc_opt var env |> Option.map Mutil.decode

let export conf base =
  assert conf.wizard;
  match
    match getenv_opt "output" conf.env with
    | Some "GED" -> Some `ged
    | Some "GW" -> Some `gw
    | _ -> None
  with
  | None -> false
  | Some output ->
      Mutil.verbose := false;
      let find_iper i =
        getenv ("i" ^ string_of_int i) conf.env |> Driver.Iper.of_string
      in
      let find_npoc i =
        let n = getenv ("n" ^ string_of_int i) conf.env in
        let p = getenv ("p" ^ string_of_int i) conf.env in
        let oc =
          match getenv_opt ("oc" ^ string_of_int i) conf.env with
          | None -> 0
          | Some i -> int_of_string i
        in
        match Geneweb_db.Driver.person_of_key base p n oc with
        | None -> raise Not_found
        | Some i -> i
      in
      let find_p i = try find_iper i with Not_found -> find_npoc i in
      let rec loop acc cnt =
        try loop (IPS.add (find_p cnt) acc) (cnt + 1) with Not_found -> acc
      in
      let ini = loop IPS.empty 1 in
      let fname =
        Driver.bname base ^ match output with `ged -> ".ged" | `gw -> ".gw"
      in
      let ipers =
        if getenv_opt "spouses" conf.env = Some "on" then
          IPS.fold
            (fun iper acc ->
              Array.fold_left
                (fun acc ifam ->
                  IPS.add (Gutil.spouse iper @@ Driver.foi base ifam) acc)
                acc
                (Driver.get_family (Driver.poi base iper)))
            ini ini
        else ini
      in
      let ipers =
        if getenv_opt "parents" conf.env = Some "on" then
          IPS.fold
            (fun iper acc ->
              match Driver.get_parents (Driver.poi base iper) with
              | None -> acc
              | Some ifam ->
                  let fam = Driver.foi base ifam in
                  IPS.add (Driver.get_father fam)
                    (IPS.add (Driver.get_mother fam) acc))
            ini ipers
        else ipers
      in
      let ipers =
        if getenv_opt "children" conf.env = Some "on" then
          IPS.fold
            (fun iper acc ->
              Array.fold_left
                (fun acc ifam ->
                  Array.fold_left
                    (fun acc iper -> IPS.add iper acc)
                    acc
                    (Driver.get_children @@ Driver.foi base ifam))
                acc
                (Driver.get_family (Driver.poi base iper)))
            ini ipers
        else ipers
      in
      let ifams =
        IPS.fold
          (fun iper acc ->
            Array.fold_left
              (fun acc ifam ->
                if
                  IFS.mem ifam acc
                  || not
                       (IPS.mem
                          (Gutil.spouse iper @@ Driver.foi base ifam)
                          ipers)
                then acc
                else IFS.add ifam acc)
              acc
              (Driver.get_family (Driver.poi base iper)))
          ipers IFS.empty
      in
      let no_notes =
        match getenv_opt "notes" conf.env with
        | None -> `none
        | Some "nn" -> `nn
        | Some "nnn" -> `nnn
        | Some _ -> `none
      in
      let source = getenv_opt "source" conf.env in
      let isolated = getenv_opt "isolated" conf.env <> Some "off" in
      let opts =
        {
          Gwexport.default_opts with
          oc = (fname, Output.print_sstring conf, Wserver.close_connection);
          no_notes;
          no_picture = getenv_opt "pictures" conf.env = Some "off";
          source;
        }
      in
      let select = ((fun i -> IPS.mem i ipers), fun i -> IFS.mem i ifams) in
      Wserver.http Def.OK;
      Wserver.header "Content-type: text/plain";
      Wserver.header
        (Printf.sprintf "Content-disposition: attachment; filename=\"%s\"" fname);
      (match output with
      | `ged -> Gwb2gedLib.gwb2ged base false opts select
      | `gw ->
          GwuLib.prepare_free_occ ~select:(fst select) base;
          Output.print_sstring conf "encoding: utf-8\n";
          Output.print_sstring conf "gwplus\n\n";
          GwuLib.gwu opts isolated base "" "" (Hashtbl.create 0) select);
      Wserver.wflush ();
      true

let () =
  Gwd_lib.GwdPlugin.register ~ns
    [ ("EXPORT", fun _assets -> w_lock @@ w_base @@ export) ]

open Geneweb
open Config
open Gwdb

let ns = "export"

module IPS = Set.Make (struct type t = Gwdb.iper let compare = compare end)
module IFS = Set.Make (struct type t = Gwdb.ifam let compare = compare end)

let w_lock = Gwd_lib.Request.w_lock ~onerror:(fun conf _ -> Update.error_locked conf ; true)
let w_base = Gwd_lib.Request.w_base ~none:(fun conf -> Hutil.incorrect_request conf ; true)

let export conf base =
  assert conf.wizard ;
  match match List.assoc_opt "output" conf.env with
    | Some "GED" -> Some `ged
    | Some "GW" -> Some `gw
    | _ -> None
  with
  | None -> false
  | Some output ->
    Mutil.verbose := false ;
    let find_iper i =
      List.assoc ("i" ^ string_of_int i) conf.env |> Gwdb.iper_of_string
    in
    let find_npoc i =
      let n = List.assoc ("n" ^ string_of_int i) conf.env |> Mutil.decode in
      let p = List.assoc ("p" ^ string_of_int i) conf.env |> Mutil.decode in
      let oc =
        match List.assoc_opt ("oc" ^ string_of_int i) conf.env with
        | None -> 0
        | Some i -> int_of_string i
      in
      match Gwdb.person_of_key base p n oc with
      | None -> raise Not_found
      | Some i -> i
    in
    let find_p i = try find_iper i with Not_found -> find_npoc i in
    let rec loop acc cnt =
      try loop (IPS.add (find_p cnt) acc) (cnt + 1)
      with Not_found -> acc
    in
    let ini = loop IPS.empty 1 in
    let fname = Gwdb.bname base ^ (match output with `ged -> ".ged" | `gw -> ".gw") in
    let ipers =
      if List.assoc_opt "spouses" conf.env = Some "on"
      then
        IPS.fold begin fun iper acc ->
          Array.fold_left begin fun acc ifam ->
            IPS.add (Gutil.spouse iper @@ foi base ifam) acc
          end acc (get_family (poi base iper))
        end ini ini
      else ini
    in
    let ipers =
      if List.assoc_opt "parents" conf.env = Some "on"
      then
        IPS.fold begin fun iper acc ->
          match get_parents (poi base iper) with
          | None -> acc
          | Some ifam ->
            let fam = foi base ifam in
            IPS.add (get_father fam) (IPS.add (get_mother fam) acc)
        end ini ipers
      else ipers
    in
    let ipers =
      if List.assoc_opt "children" conf.env = Some "on"
      then
        IPS.fold begin fun iper acc ->
          Array.fold_left begin fun acc ifam ->
            Array.fold_left begin fun acc iper ->
              IPS.add iper acc
            end acc (get_children @@ foi base ifam )
          end acc (get_family (poi base iper))
        end ini ipers
      else ipers
    in
    let ifams =
      IPS.fold begin fun iper acc ->
        Array.fold_left begin fun acc ifam ->
          if IFS.mem ifam acc || not (IPS.mem (Gutil.spouse iper @@ foi base ifam) ipers)
          then acc
          else IFS.add ifam acc
        end acc (get_family (poi base iper))
      end ipers IFS.empty
    in
    let no_notes =
      match List.assoc_opt "notes" conf.env  with
      | None -> `none
      | Some "nn" -> `nn
      | Some "nnn" -> `nnn
      | Some _ -> `none
    in
    let source = Opt.map Mutil.decode (List.assoc_opt "source" conf.env) in
    let isolated = List.assoc_opt "isolated" conf.env <> Some "off" in
    let opts =
      { Gwexport.default_opts with
        oc = fname, Output.print_string conf, ignore
      ; no_notes
      ; no_picture = List.assoc_opt "pictures" conf.env = Some "off"
      ; source
      ; base = Some (Gwdb.bname base, base)
      }
    in
    let select = ( (fun i -> IPS.mem i ipers), (fun i -> IFS.mem i ifams) ) in
    Wserver.http Def.OK ;
    Wserver.header "Content-Type: application/octet-stream" ;
    Wserver.header (Printf.sprintf "Content-Disposition: attachment; filename=\"%s\"" fname) ;
    begin match output with
      | `ged ->
        Gwb2gedLib.gwb2ged false opts select
      | `gw ->
        GwuLib.prepare_free_occ ~select:(fst select) base ;
        Output.print_string conf "encoding: utf-8\n" ;
        Output.print_string conf "gwplus\n\n" ;
        GwuLib.gwu opts isolated base "" "" (Hashtbl.create 0) select ;
    end ;
    Wserver.wflush () ;
    true

let () =
  Gwd_lib.GwdPlugin.register ~ns [ "EXPORT", fun _assets -> w_base @@ w_lock @@ export ]

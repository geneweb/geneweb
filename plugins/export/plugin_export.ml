open Geneweb
open Config

let ns = "export"

module IperSet = Set.Make (struct
    type t = Gwdb.iper
    let compare = compare
  end)

let () =
  GwdPlugin.register ~ns [ "EXPORT", fun _assets conf base ->
      let ipers =
        let rec loop acc i =
          match Util.p_getenv conf.env ("i" ^ string_of_int i) with
          | Some k -> loop (IperSet.add (Gwdb.iper_of_string k) acc) (i + 1)
          | None -> acc
        in
        loop IperSet.empty 0
      in
      let output =
        match List.assoc_opt "output" conf.env with
        | Some "GED" -> `ged
        | Some "GW" -> `gw
        | _ -> assert false
      in
      let fname = "export." ^ match output with `ged -> "ged" | `gw -> "gw" in
      let select =
        ( (fun i -> IperSet.mem i ipers)
        , (fun i ->
             let c = Gwdb.foi base i in
             IperSet.mem (Gwdb.get_father c) ipers && IperSet.mem (Gwdb.get_mother c) ipers)
        )
      in
      let opts =
        Gwexport.{ !opts with oc = fname, Wserver.woc ()
                            ; no_notes = true
                            ; base = Some (Gwdb.bname base, base)
                 }
      in
      (* no_notes not working *)
      Wserver.http Wserver.OK ;
      Wserver.header "Content-type: text/plain" ;
      Wserver.header "Content-disposition: attachment; filename=\"%s\"" fname ;
      Wserver.print_string "" ; (* Close headers section *)
      begin match output with
        | `ged ->
          Gwb2gedLib.gwb2ged false opts select
        | `gw ->
          GwuLib.prepare_free_occ ~select:(fst select) base ;
          GwuLib.gwu opts base "" "" (Hashtbl.create 0) select ;
      end ;
      Wserver.wflush () ;
      true
    ]

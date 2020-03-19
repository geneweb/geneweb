open Def
open Gwdb_driver_legacy
open Dbdisk

module J = Json

let print oc x = Printf.fprintf oc "%s\n" @@ Yojson.to_string x

let print_person oc base p a u =
  let basename = bname base in
  print oc @@
  `Assoc [ ("_key", J.key_of_iper basename p.key_index)
         ; ("basename", `String basename)
         ; ("person", J.json_of_person basename p a u)
         ]

let print_family oc base f c d =
  let basename = bname base in
  print oc @@
  `Assoc [ ("family", J.json_of_family basename f c d)
         ; ("basename", `String basename)
         ; ("_key", J.key_of_ifam basename f.fam_index) ]

let edge bname _from _to kind payload : Yojson.t =
  J.filter_out_null @@
  `Assoc [ ("_from", _from)
         ; ("_to", _to)
         ; ("kind", `String kind)
         ; ("payload", payload)
         ; ("basename", `String bname)
         ]

let handler_of_iper bname (i : iper) =
  J.handler_of_iper bname (Obj.magic i : int)

let handler_of_ifam bname (i : ifam) =
  J.handler_of_ifam bname (Obj.magic i : int)

let print_ascend oc base ip =
  let bname = bname base in
  match (base.data.ascends.get ip).parents with
  | None -> ()
  | Some ifam ->
    let f = base.data.couples.get ifam in
    let iph = handler_of_iper bname ip in
    let ifh = handler_of_ifam bname ifam in
    print oc @@ edge bname (handler_of_iper bname @@ Adef.father f) iph "ascend" ifh ;
    print oc @@ edge bname (handler_of_iper bname @@ Adef.mother f) iph "ascend" ifh

let print_unions oc base ip =
  let bname = bname base in
  let iph = handler_of_iper bname ip in
  Array.iter begin fun ifam ->
    let f = base.data.couples.get ifam in
    let from_, to_ =
      if ip = Adef.father f
      then iph, handler_of_iper bname (Adef.mother f)
      else handler_of_iper bname (Adef.father f), iph
    in
    print oc @@ edge bname from_ to_ "union" (handler_of_ifam bname ifam)
  end (base.data.unions.get ip).family

let print_relations oc base ip =
  let bname = bname base in
  let iph = handler_of_iper bname ip in
  List.iter begin fun i ->
    print oc @@ edge bname iph (handler_of_iper bname i) "related" `Null
  end (base.data.persons.get ip).related

let witness_kind = function
  | Def.Witness -> "witness"
  | Witness_GodParent -> "godparent"

let print_pevent_witness oc base ip =
  let bname = bname base in
  let iph = handler_of_iper bname ip in
  List.iter begin fun e ->
    Array.iter begin fun (i, wk) ->
      print oc @@ edge bname iph (handler_of_iper bname i) (witness_kind wk) `Null
    end e.Def.epers_witnesses
  end (base.data.persons.get ip).pevents

let print_relations oc base person =
  print_ascend oc base person ;
  print_unions oc base person ;
  print_relations oc base person ;
  print_pevent_witness oc base person

let log =
  " >> "
  ^ begin match Sys.getenv_opt "ARANGOIMP_LOG" with
    | Some x -> x
    | None -> "/dev/null"
  end
  ^ " 2>&1"

let delete progress bdir =
  let bname = Filename.(remove_extension @@ basename bdir) in
  let aux per collection =
    let query =
      {| {"query":"FOR x IN |} ^ collection ^ {| FILTER x.basename == \"|}
      ^ bname ^ {|\" REMOVE { _key: x._key } IN |} ^ collection ^ {|","ttl":0}|}
    in
    Http_curl.send
      ~tmout:0
      [ "Content-Type: application/json" ]
      (`POST query)
      (Gwdb_driver_env.url ^ "/_api/cursor")
      (fun s -> String.length s) ;
    progress `arango_disable 100 per
  in
  aux 33 "geneweb_persons" ;
  aux 66 "geneweb_families" ;
  aux 100 "geneweb_relations" ;
  Mutil.rm @@ Filename.concat bdir "use_arango"

let import ?(mem = false) progress bdir =
  let base = open_base bdir in
  let aux collection =
    Unix.open_process_out @@
    "arangoimp --file - --type jsonl "
    ^ " --server.database " ^ Gwdb_driver_env.database
    ^ " --server.username " ^ Gwdb_driver_env.user
    ^ " --server.password " ^ Gwdb_driver_env.password
    ^ " --server.endpoint tcp://" ^ Gwdb_driver_env.server ^ ":" ^ Gwdb_driver_env.port
    ^ " --collection " ^ collection
    ^ log
  in
  if not mem then begin
    load_strings_array base ;
    load_persons_array base ;
    load_ascends_array base ;
    load_unions_array base ;
  end ;
  let () =
    let oc = aux "geneweb_persons" in
    let len = Dbdisk.(base.data.persons.len) - 1 in
    for i = 0 to len do
      progress `geneweb_persons len i ;
      let p = base.data.persons.get i in
      if p.key_index <> dummy_iper then begin
        let p = Futil.map_person_ps (fun i -> i) (sou base) p in
        let a = base.data.ascends.get i in
        let u = base.data.unions.get i in
        print_person oc base p a u ;
      end
    done ;
    close_out oc
  in
  if not mem then begin
    load_families_array base ;
    load_couples_array base ;
    load_descends_array base
  end ;
  let () =
    let oc = aux "geneweb_relations" in
    let len = Dbdisk.(base.data.persons.len) - 1 in
    for i = 0 to len do
      if (base.data.persons.get i).key_index <> dummy_iper then begin
        progress `geneweb_relations len i ;
        print_relations oc base i ;
      end
    done ;
    close_out oc
  in
  if not mem then begin
    clear_persons_array base ;
    clear_ascends_array base ;
    clear_unions_array base ;
  end ;
  let () =
    let oc = aux "geneweb_families" in
    let len = Dbdisk.(base.data.families.len) - 1 in
    for i = 0 to len do
      progress `geneweb_families len i ;
      let f = base.data.families.get i in
      if f.fam_index <> dummy_ifam then begin
        let f = Futil.map_family_ps (fun i -> i) (fun i -> i) (sou base) f in
        let c = base.data.couples.get i in
        let d = base.data.descends.get i in
        print_family oc base f c d ;
      end
    done ;
    close_out oc
  in
  if not mem then begin
    clear_strings_array base ;
    clear_families_array base ;
    clear_couples_array base ;
    clear_descends_array base ;
  end ;
  Unix.close @@
  Unix.openfile
    (Filename.concat base.Dbdisk.data.Dbdisk.bdir "use_arango")
    [ Unix.O_CREAT ; Unix.O_EXCL ] 0o664

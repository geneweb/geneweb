type gwexport_charset = Ansel | Ansi | Ascii | Utf8

type gwexport_opts = {
  asc : int option;
  ascdesc : int option;
  base : (string * Gwdb.base) option;
  censor : int;
  charset : gwexport_charset;
  desc : int option;
  img_base_path : string;
  keys : string list;
  mem : bool;
  notes : bool;
  base_notes : bool;
  no_picture : bool;
  oc : string * (string -> unit) * (unit -> unit);
  parentship : bool;
  picture_path : bool;
  source : string option;
  surnames : string list;
  verbose : bool;
}

let default_opts =
  {
    asc = None;
    ascdesc = None;
    base = None;
    censor = 0;
    charset = Utf8;
    desc = None;
    img_base_path = "";
    keys = [];
    mem = false;
    notes = true;
    base_notes = true;
    no_picture = false;
    oc = ("", print_string, fun () -> close_out stdout);
    parentship = false;
    picture_path = false;
    source = None;
    surnames = [];
    verbose = false;
  }

let errmsg = "Usage: " ^ Sys.argv.(0) ^ " <BASE> [OPT]"

let anonfun c s =
  if !c.base = None then (
    Secure.set_base_dir (Filename.dirname s);
    c := { !c with base = Some (s, Gwdb.open_base s) })
  else raise (Arg.Bad "Cannot treat several databases")

let speclist c =
  let int_arg ?(check = Fun.const @@ Ok ()) ?(symbolic_values = []) continue =
    Arg.String
      (fun arg ->
        let bad_arg message =
          Arg.Bad
            (Printf.sprintf "option '%s': %s" Sys.argv.(!Arg.current) message)
        in
        let arg =
          match int_of_string_opt arg with
          | Some arg -> Ok arg
          | None ->
              let error_message =
                let symbolic_values =
                  symbolic_values |> List.map fst
                  |> List.map (Printf.sprintf "'%s'")
                in
                Printf.sprintf
                  "unknown symbolic value '%s', possible symbolic values are %s"
                  arg
                  (String.concat " " symbolic_values)
              in
              Option.to_result ~none:error_message
                (List.assoc_opt arg symbolic_values)
        in
        let ( >>= ) = Result.bind in
        Result.fold ~ok:continue
          ~error:(fun message -> raise @@ bad_arg message)
          (arg >>= check >>= Fun.const arg))
  in
  let positive_int_arg ?symbolic_values continue =
    int_arg ?symbolic_values
      ~check:(fun arg ->
        if arg > 0 then Ok () else Error "positive integer expected")
      continue
  in
  [
    ( "-a",
      positive_int_arg
        ~symbolic_values:[ ("all", max_int) ]
        (fun s -> c := { !c with asc = Some s }),
      "[<N>|all] maximum generation of the root's ascendants" );
    ( "-ad",
      int_arg
        ~symbolic_values:[ ("all", -max_int) ]
        (fun s -> c := { !c with ascdesc = Some s }),
      "[<N>|all] maximum generation of the root's ascendants descendants. The \
       value is relative to the root person. Thus, for example, '0' means \
       *down to the level of the root person*, '2' means *down to the level of \
       the grandparents of the root person* and '-2' means *down to the level \
       of the grandchildren of the root person*." );
    ( "-key",
      Arg.String (fun s -> c := { !c with keys = s :: !c.keys }),
      "<KEY> key reference of person. Used for -a/-d/-ad/-parentship options. \
       Can be used multiple times. Key format is \"First Name.occ SURNAME\"" );
    ( "-c",
      Arg.Int (fun s -> c := { !c with censor = s }),
      "<NUM>: when a person is born less than <num> years ago, it is not \
       exported unless it is Public. All the spouses and descendants are also \
       censored." );
    ( "-charset",
      Arg.String
        (fun s ->
          c :=
            {
              !c with
              charset =
                (match s with
                | "ASCII" -> Ascii
                | "ANSEL" -> Ansel
                | "ANSI" -> Ansi
                | "UTF-8" -> Utf8
                | _ -> raise (Arg.Bad "bad -charset value"));
            }),
      "[ASCII|ANSEL|ANSI|UTF-8] set charset; default is UTF-8" );
    ( "-d",
      positive_int_arg
        ~symbolic_values:[ ("all", max_int) ]
        (fun s -> c := { !c with desc = Some s }),
      "[<N>|all] maximum generation of the root's descendants." );
    ( "-mem",
      Arg.Unit (fun () -> c := { !c with mem = true }),
      " save memory space, but slower." );
    ( "-no-base-notes",
      Arg.Unit (fun () -> c := { !c with base_notes = false }),
      " no database notes." );
    ( "-nn",
      Arg.Unit (fun () -> c := { !c with base_notes = false }),
      " no database notes." );
    ( "-no-notes",
      Arg.Unit (fun () -> c := { !c with notes = false; base_notes = false }),
      " no notes (implies -no-base-notes)." );
    ( "-nnn",
      Arg.Unit (fun () -> c := { !c with notes = false; base_notes = false }),
      " no notes (implies -no-base-notes)." );
    ( "-no-picture",
      Arg.Unit (fun () -> c := { !c with no_picture = true }),
      " don't extract individual picture." );
    ( "-nopicture",
      Arg.Unit (fun () -> c := { !c with no_picture = true }),
      " don't extract individual picture." );
    ( "-o",
      Arg.String
        (fun s ->
          let oc = open_out s in
          c := { !c with oc = (s, output_string oc, fun () -> close_out oc) }),
      "<GED> output file name (default: stdout)." );
    ( "-parentship",
      Arg.Unit (fun () -> c := { !c with parentship = true }),
      " select individuals involved in parentship computation between pairs of \
       keys. Pairs must be defined with -key option, descendant first: e.g. \
       -key \"Descendant.0 SURNAME\" -key \"Ancestor.0 SURNAME\". If multiple \
       pair are provided, union of persons are returned." );
    ( "-picture-path",
      Arg.Unit (fun () -> c := { !c with picture_path = true }),
      " extract pictures path." );
    ( "-s",
      Arg.String (fun x -> c := { !c with surnames = x :: !c.surnames }),
      "<SN> select this surname (option usable several times, union of \
       surnames will be used)." );
    ( "-source",
      Arg.String (fun x -> c := { !c with source = Some x }),
      "<SRC> replace individuals and families sources. Also delete event \
       sources." );
    ("-v", Arg.Unit (fun () -> c := { !c with verbose = true }), " verbose");
  ]

(* S: Does it mean private persons whose birth year is before 'max_year'
   are uncensored? *)

(* TODO this should not be redefined here *)

(** [is_censored_person max_year person_name] Returns [true] iff the person has
    a birth date that is after max_year and its visibility is not public *)
let is_censored_person threshold p =
  match Date.cdate_to_dmy_opt (Gwdb.get_birth p) with
  | None -> false
  | Some dmy -> dmy.year >= threshold && Gwdb.get_access p != Def.Public

(** [is_censored_couple base max_year family] Returns [true] if either the
    father or the mother of a given family in the base is censored *)
let is_censored_couple base threshold cpl =
  (is_censored_person threshold @@ Gwdb.poi base (Gwdb.get_father cpl))
  || (is_censored_person threshold @@ Gwdb.poi base (Gwdb.get_mother cpl))

(* The following functions are utils set people as "censored" by marking them.
   Censoring a person consists in setting a mark defined as:
   `Marker.get pmark p lor flag`

   This gets the current mark, being 0 or 1, and `lor`s it with `flag` which is `1`.
   TODO: replace integer markers by booleans
*)

(** Marks a censored person *)
let censor_person base pmark flag threshold p no_check =
  let ps = Gwdb.poi base p in
  if no_check || is_censored_person threshold ps then
    Gwdb.Marker.set pmark p (Gwdb.Marker.get pmark p lor flag)

(** Marks all the members of a family that are censored. If a couple is
    censored, its parents and all its descendants are marked. *)
let rec censor_family base pmark fmark flag threshold i no_check =
  let censor_unions p =
    let uni = Gwdb.poi base p in
    Array.iter
      (fun ifam ->
        censor_family base pmark fmark flag threshold ifam true;
        censor_person base pmark flag threshold p true)
      (Gwdb.get_family uni)
  in
  let censor_descendants f =
    let des = Gwdb.foi base f in
    Array.iter
      (fun iper -> if Gwdb.Marker.get pmark iper = 0 then censor_unions iper)
      (Gwdb.get_children des)
  in
  let all_families_censored p =
    (* FIXME: replace with forall *)
    let uni = Gwdb.poi base p in
    Array.fold_left
      (fun check ifam -> check && Gwdb.Marker.get fmark ifam = 0)
      true (Gwdb.get_family uni)
  in
  let censor_spouse iper =
    if all_families_censored iper then
      Gwdb.Marker.set pmark iper (Gwdb.Marker.get pmark iper lor flag)
    (* S: Replace this line by `censor_person`? *)
  in
  if Gwdb.Marker.get fmark i = 0 then
    let fam = Gwdb.foi base i in
    if no_check || is_censored_couple base threshold fam then (
      Gwdb.Marker.set fmark i (Gwdb.Marker.get fmark i lor flag);
      censor_spouse (Gwdb.get_father fam);
      censor_spouse (Gwdb.get_mother fam);
      censor_descendants i)

(** Marks all the families that are censored in the given base. *)
let censor_base base pmark fmark flag threshold =
  Gwdb.Collection.iter
    (fun i -> censor_family base pmark fmark flag threshold i false)
    (Gwdb.ifams base);
  Gwdb.Collection.iter
    (fun i -> censor_person base pmark flag threshold i false)
    (Gwdb.ipers base)

(** Set non visible persons and families as censored *)
let restrict_base base per_tab fam_tab flag =
  (* Starts by censoring non visible persons of the base *)
  Gwdb.Collection.iter
    (fun i ->
      if Gwdb.base_visible_get base (fun _ -> false) i then
        Gwdb.Marker.set per_tab i (Gwdb.Marker.get per_tab i lor flag))
      (* S: replace by `censor_person` ? *)
    (Gwdb.ipers base);

  Gwdb.Collection.iter
    (fun i ->
      let fam = Gwdb.foi base i in
      let des_visible =
        (* There exists a children of the family that is not censored *)
        (* FIXME: replace with exists *)
        Array.fold_left
          (fun check iper -> check || Gwdb.Marker.get per_tab iper = 0)
          false (Gwdb.get_children fam)
      in
      let cpl_not_visible =
        (* Father or mother is censored *)
        Gwdb.Marker.get per_tab (Gwdb.get_father fam) <> 0
        || Gwdb.Marker.get per_tab (Gwdb.get_mother fam) <> 0
      in
      (* If all the children, father and mother are censored, then censor family *)
      if (not des_visible) && cpl_not_visible then
        Gwdb.Marker.set fam_tab i (Gwdb.Marker.get fam_tab i lor flag))
    (Gwdb.ifams base)

(** [select_asc conf base max_gen ips] Returns all the ancestors of persons in
    the list `ips` up to the `max_gen` generation. *)
let select_asc conf base max_gen ips =
  let rec loop_asc (gen : int) set ip =
    if not @@ Gwdb.IperSet.mem ip set then
      let set = Gwdb.IperSet.add ip set in
      let p = Geneweb.Util.pget conf base ip in
      if gen < max_gen then
        match Gwdb.get_parents p with
        | Some ifam ->
            let cpl = Gwdb.foi base ifam in
            let set = loop_asc (gen + 1) set (Gwdb.get_father cpl) in
            loop_asc (gen + 1) set (Gwdb.get_mother cpl)
        | None -> set
      else set
    else set
  in
  List.fold_left (loop_asc 0) Gwdb.IperSet.empty ips

(* S: only used by `select_surnames` in a List.iter *)
(* Should it use search engine functions? *)

(** [select_surname nase pmark fmark surname] Sets a `true` marker to families
    whose mother or father that match the given surname. Propagates the mark to
    children that have this surname. *)
let select_surname base pmark fmark surname =
  let surname = Name.strip_lower surname in
  Gwdb.Collection.iter
    (fun i ->
      let fam = Gwdb.foi base i in
      let fath = Gwdb.poi base (Gwdb.get_father fam) in
      let moth = Gwdb.poi base (Gwdb.get_mother fam) in
      if
        Name.strip_lower (Gwdb.sou base (Gwdb.get_surname fath)) = surname
        || Name.strip_lower (Gwdb.sou base (Gwdb.get_surname moth)) = surname
      then (
        Gwdb.Marker.set fmark i true;
        Gwdb.Marker.set pmark (Gwdb.get_father fam) true;
        Gwdb.Marker.set pmark (Gwdb.get_mother fam) true;
        Array.iter
          (fun ic ->
            let p = Gwdb.poi base ic in
            if
              (not (Gwdb.Marker.get pmark ic))
              && Name.strip_lower (Gwdb.sou base (Gwdb.get_surname p)) = surname
            then Gwdb.Marker.set pmark ic true)
          (Gwdb.get_children fam)))
    (Gwdb.ifams base)

(** [select_surnames base surnames] Calls `select_surname` on every family that
    have the given surnames. Returns two functions: * the first takes a person
    and returns `true` iff it has been selected * the second takes a family and
    returns `false` iff it has been selected *)
let select_surnames base surnames : (Gwdb.iper -> bool) * (Gwdb.ifam -> bool) =
  let pmark = Gwdb.iper_marker (Gwdb.ipers base) false in
  let fmark = Gwdb.ifam_marker (Gwdb.ifams base) false in
  List.iter (select_surname base pmark fmark) surnames;
  ((fun i -> Gwdb.Marker.get pmark i), fun i -> Gwdb.Marker.get fmark i)

(**/**)

(** [select_parentship base ip1 ip2] Returns the set of common descendants of
    ip1 and the ancestors of ip2 and the set of their families. *)
let select_parentship base ip1 ip2 =
  let conf =
    Geneweb.Config.{ empty with wizard = true; bname = Gwdb.bname base }
  in
  let asc = select_asc conf base max_int [ ip1 ] in
  let desc = Geneweb.Util.select_desc conf base (-max_int) [ (ip2, 0) ] in
  let ipers =
    (* S: The intersection of asc and desc *)
    if Gwdb.IperSet.cardinal asc > Hashtbl.length desc then
      Hashtbl.fold
        (fun k _ acc ->
          if Gwdb.IperSet.mem k asc then Gwdb.IperSet.add k acc else acc)
        desc Gwdb.IperSet.empty
    else
      Gwdb.IperSet.fold
        (fun k acc ->
          if Hashtbl.mem desc k then Gwdb.IperSet.add k acc else acc)
        asc Gwdb.IperSet.empty
  in
  let ifams =
    (* S: families  *)
    Gwdb.IperSet.fold
      (fun iper acc ->
        Array.fold_left
          (fun acc ifam ->
            if
              Gwdb.IfamSet.mem ifam acc (* S: useless test? *)
              || not
                   (Gwdb.IperSet.mem
                      (Gutil.spouse iper @@ Gwdb.foi base ifam)
                      ipers)
              (* S: is the partner of the
                 person not in ipers? *)
            then acc
            else Gwdb.IfamSet.add ifam acc)
          acc
          (Gwdb.get_family (Gwdb.poi base iper)))
      ipers Gwdb.IfamSet.empty
  in
  (ipers, ifams)

(** [select_from_set ipers ifams] Returns two functions : * the first returns
    true if its input is in ipers * the second returns true if its input is in
    ifams *)
let select_from_set (ipers : Gwdb.IperSet.t) (ifams : Gwdb.IfamSet.t) =
  let sel_per i = Gwdb.IperSet.mem i ipers in
  let sel_fam i = Gwdb.IfamSet.mem i ifams in
  (sel_per, sel_fam)

let check_options options =
  let conditional_check ~condition check =
    if not @@ condition () then Ok () else check ()
  in
  let check_base () =
    if Option.is_some options.base then Ok () else Error "Missing base name."
  in
  let check_root_person_keys () =
    let options_requiring_key =
      [ options.asc; options.desc; options.ascdesc ]
    in
    if options.keys <> [] || List.for_all Option.is_none options_requiring_key
    then Ok ()
    else Error "Missing root person."
  in
  let check_parentship_keys () =
    if options.keys <> [] && List.length options.keys mod 2 = 0 then Ok ()
    else Error "Missing person."
  in
  let ( >>= ) = Result.bind in
  check_base () >>= check_root_person_keys >>= fun () ->
  conditional_check
    ~condition:(fun () -> options.parentship)
    check_parentship_keys

let select opts =
  let () =
    Result.iter_error
      (fun error_message ->
        raise
        @@ Arg.Bad
             (Printf.sprintf "%s Use option -help for usage" error_message))
      (check_options opts)
  in
  match opts.base with
  | None -> assert false
  | Some (_, base) ->
      let ips = List.filter_map (Gutil.person_of_string_key base) opts.keys in
      let not_censor_p, not_censor_f =
        if opts.censor <> 0 then (
          let pmark = Gwdb.iper_marker (Gwdb.ipers base) 0 in
          let fmark = Gwdb.ifam_marker (Gwdb.ifams base) 0 in
          (if opts.censor = -1 then restrict_base base pmark fmark 1
           else
             let tm = Unix.localtime (Unix.time ()) in
             let threshold = 1900 + tm.Unix.tm_year - opts.censor in
             censor_base base pmark fmark 1 threshold);
          ( (fun i -> Gwdb.Marker.get pmark i = 0),
            fun i -> Gwdb.Marker.get fmark i = 0 ))
        else ((fun _ -> true), fun _ -> true)
      in
      let conf = Geneweb.Config.{ empty with wizard = true } in
      let sel_per, sel_fam =
        (* S: a lot of redundant tests are done here, would be simpler with
           pattern matchings and factorization. *)
        if opts.ascdesc <> None || opts.desc <> None then (
          assert (opts.censor = 0);
          let asc =
            if opts.ascdesc <> None then Option.value ~default:max_int opts.asc
            else Option.value ~default:0 opts.asc
          in
          let desc = -Option.value ~default:0 opts.desc in
          let ht =
            match opts.ascdesc with
            | Some ascdesc ->
                let ips = List.map (fun i -> (i, asc)) ips in
                let skip_descendants ~ancestors ~generation _ =
                  let is_descendant_of_root_person () =
                    let root_persons =
                      ips |> List.map fst |> Gwdb.IperSet.of_list
                    in
                    not @@ Gwdb.IperSet.is_empty
                    @@ Gwdb.IperSet.inter ancestors root_persons
                  in
                  generation <= desc && is_descendant_of_root_person ()
                in
                Geneweb.Util.select_mascdesc ~skip_descendants conf base ips
                  ascdesc
            | None ->
                let ht = Hashtbl.create 0 in
                Gwdb.IperSet.iter
                  (fun i -> Hashtbl.add ht i (Gwdb.poi base i))
                  (select_asc conf base asc ips);
                ht
          in
          let ht' =
            let ips = List.map (fun i -> (i, 0)) ips in
            Geneweb.Util.select_desc conf base desc ips
          in
          Hashtbl.iter (fun i p -> Hashtbl.replace ht i p) ht';
          let ipers =
            Hashtbl.fold
              (fun i _ ipers -> Gwdb.IperSet.add i ipers)
              ht Gwdb.IperSet.empty
          in
          let ifams =
            Gwdb.IperSet.fold
              (fun iper acc ->
                Array.fold_left
                  (fun acc ifam ->
                    if
                      Gwdb.IfamSet.mem ifam acc
                      || not
                           (Gwdb.IperSet.mem
                              (Gutil.spouse iper @@ Gwdb.foi base ifam)
                              ipers)
                    then acc
                    else Gwdb.IfamSet.add ifam acc)
                  acc
                  (Gwdb.get_family (Gwdb.poi base iper)))
              ipers Gwdb.IfamSet.empty
          in
          let sel_per i = Gwdb.IperSet.mem i ipers in
          let sel_fam i = Gwdb.IfamSet.mem i ifams in
          (sel_per, sel_fam))
        else
          match opts.asc with
          | Some asc ->
              let ipers = select_asc conf base asc ips in
              let per_sel i = Gwdb.IperSet.mem i ipers in
              let fam_sel i =
                let f = Gwdb.foi base i in
                per_sel (Gwdb.get_father f) && per_sel (Gwdb.get_mother f)
              in
              (per_sel, fam_sel)
          | None ->
              if opts.surnames <> [] then select_surnames base opts.surnames
              else if opts.parentship then
                let rec loop ipers ifams = function
                  | [] -> select_from_set ipers ifams
                  | k2 :: k1 :: tl ->
                      let ipers', ifams' = select_parentship base k1 k2 in
                      let ipers =
                        Gwdb.IperSet.fold Gwdb.IperSet.add ipers ipers'
                      in
                      let ifams =
                        Gwdb.IfamSet.fold Gwdb.IfamSet.add ifams ifams'
                      in
                      loop ipers ifams tl
                  | [ _ ] -> assert false
                in
                loop Gwdb.IperSet.empty Gwdb.IfamSet.empty ips
              else
                ( (fun person_id ->
                    ips = [] || List.exists (Gwdb.eq_iper person_id) ips),
                  fun _ -> ips = [] )
      in
      ( (fun i -> not_censor_p i && sel_per i),
        fun i -> not_censor_f i && sel_fam i )

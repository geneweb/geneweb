open Geneweb
module Collection = Geneweb_db.Collection
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

type gwexport_charset = Ansel | Ansi | Ascii | Utf8

type gwexport_opts = {
  asc : int option;
  ascdesc : int option;
  censor : int;
  charset : gwexport_charset;
  desc : int option;
  img_base_path : string;
  keys : string list;
  aws : bool;
  mem : bool;
  no_notes : [ `none | `nn | `nnn ];
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
    censor = 0;
    charset = Utf8;
    desc = None;
    img_base_path = "";
    keys = [];
    aws = false;
    mem = false;
    no_notes = `none;
    no_picture = false;
    oc = ("", prerr_string, fun () -> close_out stderr);
    parentship = false;
    picture_path = false;
    source = None;
    surnames = [];
    verbose = false;
  }

let errmsg = "Usage: " ^ Sys.argv.(0) ^ " <BASE> [OPT]"

let speclist c =
  [
    ( "-a",
      Arg.Int (fun s -> c := { !c with asc = Some s }),
      "<N> maximum generation of the root's ascendants" );
    ( "-ad",
      Arg.Int (fun s -> c := { !c with ascdesc = Some s }),
      "<N> maximum generation of the root's ascendants descendants \
       (deprecated, use -a with -d)" );
    ( "-key",
      Arg.String (fun s -> c := { !c with keys = s :: !c.keys }),
      "<KEY> key reference of root person. Used for -a/-d options. Can be used \
       multiple times. Key format is \"First_Name.occ SURNAME\"" );
    ( "-aws",
      Arg.Unit (fun () -> c := { !c with aws = true }),
      " save siblings of exported persons." );
    ( "-c",
      Arg.Int (fun s -> c := { !c with censor = s }),
      "<NUM> when a person is born less than <num> years ago, it is not \
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
      " [ASCII|ANSEL|ANSI|UTF-8] set charset; default is UTF-8" );
    ( "-d",
      Arg.Int (fun s -> c := { !c with desc = Some (-s) }),
      "<N> maximum generation of the root's descendants." );
    ( "-mem",
      Arg.Unit (fun () -> c := { !c with mem = true }),
      " save memory space, but slower." );
    ( "-nn",
      Arg.Unit
        (fun () -> if !c.no_notes = `none then c := { !c with no_notes = `nn }),
      " no (database) notes." );
    ( "-nnn",
      Arg.Unit (fun () -> c := { !c with no_notes = `nnn }),
      " no notes (implies -nn)." );
    ( "-nopicture",
      Arg.Unit (fun () -> c := { !c with no_picture = true }),
      " don't extract individual picture." );
    ( "-o",
      Arg.String
        (fun s ->
          let oc = open_out s in
          c := { !c with oc = (s, output_string oc, fun () -> close_out oc) }),
      "<FILE> output file name (default: stdout)." );
    ( "-parentship",
      Arg.Unit (fun () -> c := { !c with parentship = true }),
      " select individuals involved in parentship computation between pairs of \
       keys. Pairs must be defined with -key option, descendant first: e.g. \
       -key \"Descendant.0 SURNAME\" -key \"Ancestor.0 SURNAME\". If multiple \
       pair are provided, union of persons are returned." );
    ( "-picture-path",
      Arg.Unit (fun () -> c := { !c with picture_path = true }),
      " extract pictures path." );
    ( "-sn",
      Arg.String (fun x -> c := { !c with surnames = x :: !c.surnames }),
      "<SN> select this surname (option usable several times, union of \
       surnames will be used)." );
    ( "-source",
      Arg.String (fun x -> c := { !c with source = Some x }),
      "<SRC> replace individuals and families sources. Also delete event \
       sources." );
    ("-v", Arg.Unit (fun () -> c := { !c with verbose = true }), " verbose");
  ]

module IPS = Geneweb_db.Driver.Iper.Set
module IFS = Geneweb_db.Driver.Ifam.Set

(** [is_censored_person base threshold p] détermine si une personne non Public
    doit être censurée selon le seuil temporel pour des raisons de
    confidentialité.

    Cette fonction utilise des heuristiques pour déterminer si une personne est
    probablement vivante :
    - Si née après le seuil : censurée
    - Si pas de date de naissance mais décédée récemment : censurée
    - Si vivante ou statut inconnu : censurée par précaution *)
let is_censored_person threshold p =
  (* Vérifier la date de naissance *)
  if Driver.get_access p = Def.Public then false
  else
    match Date.cdate_to_dmy_opt (Driver.get_birth p) with
    | Some dmy -> dmy.Adef.year > threshold
    | None -> (
        (* Si pas de date de naissance, vérifier la date de décès *)
        match Driver.get_death p with
        | Death (_, cd) -> (
            match Date.cdate_to_dmy_opt cd with
            | Some dmy ->
                (* Heuristique : si décédé après (seuil - 80 ans), peut-être vivant *)
                dmy.year > threshold - 80
            | None -> true (* Pas de date de décès : supposer vivant *))
        | NotDead -> true (* Vivant *)
        | DontKnowIfDead | DeadDontKnowWhen ->
            (* Incertain : appliquer la censure par précaution *)
            true
        | _ -> false)

(** [is_censored_couple base max_year family] Returns [true] if either the
    father or the mother of a given family in the base is censored *)
let is_censored_couple base threshold cpl =
  (is_censored_person threshold @@ Driver.poi base (Driver.get_father cpl))
  || is_censored_person threshold
     @@ Driver.poi base (Driver.get_mother cpl)

(* The following functions are utils set people as "censored" by marking them.
   Censoring a person consists in setting a mark defined as:
   `Collection.Marker.get pmark p lor flag`

   This gets the current mark, being 0 or 1, and `lor`s it with `flag` which is `1`.
   TODO: replace integer markers by booleans
*)

(** Marks a censored person *)
let censor_person base pmark flag threshold p no_check =
  let ps = Driver.poi base p in
  if no_check || is_censored_person threshold ps then
    Collection.Marker.set pmark p (Collection.Marker.get pmark p lor flag)

(** Marks all the members of a family that are censored. If a couple is
    censored, its parents and all its descendants are marked. *)
let rec censor_family base pmark fmark flag threshold i no_check =
  let censor_unions p =
    let uni = Driver.poi base p in
    Array.iter
      (fun ifam ->
        censor_family base pmark fmark flag threshold ifam true;
        censor_person base pmark flag threshold p true)
      (Driver.get_family uni)
  in
  let censor_descendants f =
    let des = Driver.foi base f in
    Array.iter
      (fun iper ->
        if Collection.Marker.get pmark iper = 0 then censor_unions iper)
      (Driver.get_children des)
  in
  let all_families_censored p =
    (* FIXME: replace with forall *)
    let uni = Driver.poi base p in
    Array.fold_left
      (fun check ifam -> check && Collection.Marker.get fmark ifam = 0)
      true (Driver.get_family uni)
  in
  let censor_spouse iper =
    if all_families_censored iper then
      Collection.Marker.set pmark iper
        (Collection.Marker.get pmark iper lor flag)
    (* S: Replace this line by `censor_person`? *)
  in
  if Collection.Marker.get fmark i = 0 then
    let fam = Driver.foi base i in
    if no_check || is_censored_couple base threshold fam then (
      Collection.Marker.set fmark i (Collection.Marker.get fmark i lor flag);
      censor_spouse (Driver.get_father fam);
      censor_spouse (Driver.get_mother fam);
      censor_descendants i)

(** Marks all the families that are censored in the given base. *)
let censor_base base pmark fmark flag threshold =
  Collection.iter
    (fun i -> censor_family base pmark fmark flag threshold i false)
    (Driver.ifams base);
  Collection.iter
    (fun i -> censor_person base pmark flag threshold i false)
    (Driver.ipers base)

(** Set non visible persons and families as censored *)
let restrict_base base per_tab fam_tab flag =
  (* Starts by censoring non visible persons of the base *)
  Collection.iter
    (fun i ->
      if Driver.base_visible_get base (fun _ -> false) i then
        Collection.Marker.set per_tab i
          (Collection.Marker.get per_tab i lor flag))
      (* S: replace by `censor_person` ? *)
    (Driver.ipers base);

  Collection.iter
    (fun i ->
      let fam = Driver.foi base i in
      let des_visible =
        (* There exists a children of the family that is not censored *)
        (* FIXME: replace with exists *)
        Array.fold_left
          (fun check iper -> check || Collection.Marker.get per_tab iper = 0)
          false (Driver.get_children fam)
      in
      let cpl_not_visible =
        (* Father or mother is censored *)
        Collection.Marker.get per_tab (Driver.get_father fam) <> 0
        || Collection.Marker.get per_tab (Driver.get_mother fam) <> 0
      in
      (* If all the children, father and mother are censored, then censor family *)
      if (not des_visible) && cpl_not_visible then
        Collection.Marker.set fam_tab i
          (Collection.Marker.get fam_tab i lor flag))
    (Driver.ifams base)

(** [select_asc conf base max_gen ips] Returns all the ancestors of persons in
    the list `ips` up to the `max_gen` generation. *)
let select_asc conf base max_gen ips =
  let rec loop_asc (gen : int) set ip =
    if not @@ IPS.mem ip set then
      let set = IPS.add ip set in
      let p = Util.pget conf base ip in
      if gen < max_gen then
        match Driver.get_parents p with
        | Some ifam ->
            let cpl = Driver.foi base ifam in
            let set = loop_asc (gen + 1) set (Driver.get_father cpl) in
            loop_asc (gen + 1) set (Driver.get_mother cpl)
        | _ -> set
      else set
    else set
  in
  List.fold_left (loop_asc 0) IPS.empty ips

(* S: only used by `select_surnames` in a List.iter *)
(* Should it use search engine functions? *)

(** [select_surname base pmark fmark surname] Sets a `true` marker to families
    whose mother or father that match the given surname. Propagates the mark to
    children that have this surname. *)
let select_surname base pmark fmark surname =
  let surname = Name.strip_lower surname in
  Collection.iter
    (fun i ->
      let fam = Driver.foi base i in
      let fath = Driver.poi base (Driver.get_father fam) in
      let moth = Driver.poi base (Driver.get_mother fam) in
      if
        Name.strip_lower (Driver.sou base (Driver.get_surname fath)) = surname
        || Name.strip_lower (Driver.sou base (Driver.get_surname moth))
           = surname
      then (
        Collection.Marker.set fmark i true;
        Collection.Marker.set pmark (Driver.get_father fam) true;
        Collection.Marker.set pmark (Driver.get_mother fam) true;
        Array.iter
          (fun ic ->
            let p = Driver.poi base ic in
            if
              (not (Collection.Marker.get pmark ic))
              && Name.strip_lower (Driver.sou base (Driver.get_surname p))
                 = surname
            then Collection.Marker.set pmark ic true)
          (Driver.get_children fam)))
    (Driver.ifams base)

(** [select_surnames base surnames] Calls `select_surname` on every family that
    have the given surnames. Returns two functions:
    - the first takes a person and returns `true` iff it has been selected
    - the second takes a family and returns `true` iff it has been selected *)
let select_surnames base surnames :
    (Driver.iper -> bool) * (Driver.ifam -> bool) =
  let pmark = Driver.iper_marker (Driver.ipers base) false in
  let fmark = Driver.ifam_marker (Driver.ifams base) false in
  List.iter (select_surname base pmark fmark) surnames;
  ( (fun i -> Collection.Marker.get pmark i),
    fun i -> Collection.Marker.get fmark i )

(** [select_parentship base ip1 ip2] Returns the set of common descendants of
    ip1 and the ancestors of ip2 and the set of their families. *)
let select_parentship base ip1 ip2 =
  let conf = Config.{ empty with wizard = true; bname = Driver.bname base } in
  let asc = select_asc conf base max_int [ ip1 ] in
  let desc = Util.select_desc conf base (-max_int) [ (ip2, 0) ] in
  let ipers =
    (* S: The intersection of asc and desc *)
    if IPS.cardinal asc > Hashtbl.length desc then
      Hashtbl.fold
        (fun k _ acc -> if IPS.mem k asc then IPS.add k acc else acc)
        desc IPS.empty
    else
      IPS.fold
        (fun k acc -> if Hashtbl.mem desc k then IPS.add k acc else acc)
        asc IPS.empty
  in
  let ifams =
    IPS.fold
      (fun iper acc ->
        Array.fold_left
          (fun acc ifam ->
            if
              IFS.mem ifam acc
              || not (IPS.mem (Gutil.spouse iper @@ Driver.foi base ifam) ipers)
            then acc
            else IFS.add ifam acc)
          acc
          (Driver.get_family (Driver.poi base iper)))
      ipers IFS.empty
  in
  (ipers, ifams)

(** Types pour clarifier les différentes stratégies de sélection *)
type selection_strategy =
  | SelectAll  (** Sélectionner toutes les personnes *)
  | SelectSurnames of string list  (** Filtrer par noms de famille *)
  | SelectParentship of Driver.iper list  (** Chemins de parenté entre paires *)
  | SelectAncestors of int  (** Ascendants avec profondeur *)
  | SelectDescendants of int  (** Descendants avec profondeur *)
  | SelectAncDesc of { asc : int; desc : int }  (** Ascendants ET descendants *)

(** [determine_strategy opts ips] analyse les options et détermine la stratégie
    de sélection à appliquer.

    Note: L'option [ascdesc] est conservée pour compatibilité mais l'utilisation
    de [asc] + [desc] est recommandée. *)
let determine_strategy opts ips =
  match (opts.ascdesc, opts.asc, opts.desc, opts.surnames, opts.parentship) with
  (* ascdesc : syntaxe legacy, équivalent à asc + desc *)
  | Some ascdesc, _, _, _, _ ->
      SelectAncDesc { asc = ascdesc; desc = ascdesc }
  (* asc + desc : syntaxe recommandée *)
  | None, Some asc, Some desc, _, _ ->
      SelectAncDesc { asc; desc }
  (* asc seul *)
  | None, Some asc, None, _, _ -> SelectAncestors asc
  (* desc seul *)
  | None, None, Some desc, _, _ -> SelectDescendants desc
  (* Noms de famille *)
  | None, None, None, surnames, _ when surnames <> [] -> SelectSurnames surnames
  (* Parenté *)
  | None, None, None, [], true -> SelectParentship ips
  (* Aucun filtre *)
  | None, None, None, _, _ -> SelectAll

(** [build_person_and_family_sets base ht] construit les ensembles de personnes
    et familles à partir d'une hashtable de personnes sélectionnées.

    Pour chaque famille où au moins un conjoint est sélectionné, ajoute aussi
    l'autre conjoint aux personnes sélectionnées pour avoir des familles complètes.

    @return
      Triple (sel_per, sel_fam, (ipers, ifams)) où:
      - sel_per: prédicat de sélection des personnes
      - sel_fam: prédicat de sélection des familles
      - ipers: ensemble des indices de personnes sélectionnées (+ leurs conjoints)
      - ifams: ensemble des indices de familles sélectionnées *)
let build_person_and_family_sets base ht =
  let ipers = Hashtbl.fold (fun i _ ipers -> IPS.add i ipers) ht IPS.empty in
  (* Sélectionner les familles et ajouter les conjoints *)
  let ipers, ifams =
    IPS.fold
      (fun iper (ipers_acc, ifams_acc) ->
        Array.fold_left
          (fun (ipers_acc, ifams_acc) ifam ->
            if IFS.mem ifam ifams_acc then (ipers_acc, ifams_acc)
            else
              let fam = Driver.foi base ifam in
              let spouse = Gutil.spouse iper fam in
              (* Ajouter la famille et le conjoint *)
              let ipers_acc = IPS.add spouse ipers_acc in
              let ifams_acc = IFS.add ifam ifams_acc in
              (ipers_acc, ifams_acc))
          (ipers_acc, ifams_acc)
          (Driver.get_family (Driver.poi base iper)))
      ipers (ipers, IFS.empty)
  in
  let sel_per i = IPS.mem i ipers in
  let sel_fam i = IFS.mem i ifams in
  (sel_per, sel_fam, (ipers, ifams))

(** [add_siblings base ipers ifams] ajoute les frères et sœurs de toutes les
    personnes dans ipers, ainsi que leur famille parentale commune et leurs parents.

    Cela ajoute implicitement un niveau d'ascendance pour pouvoir visualiser
    le lien de fratrie.

    @return Nouvelle paire (ipers, ifams) avec les siblings, parents et familles ajoutés *)
let add_siblings base ipers ifams =
  (* Pour chaque personne, ajouter ses siblings, sa famille parentale et ses parents *)
  let new_ipers, new_ifams =
    IPS.fold (fun iper (acc_ipers, acc_ifams) ->
      let p = Driver.poi base iper in
      match Driver.get_parents p with
      | Some parent_ifam ->
          let fam = Driver.foi base parent_ifam in
          (* Ajouter la famille parentale *)
          let acc_ifams = IFS.add parent_ifam acc_ifams in
          (* Ajouter les parents (père et mère) *)
          let acc_ipers = IPS.add (Driver.get_father fam) acc_ipers in
          let acc_ipers = IPS.add (Driver.get_mother fam) acc_ipers in
          (* Ajouter tous les enfants (siblings) *)
          let acc_ipers = Array.fold_left (fun acc child_iper ->
            IPS.add child_iper acc
          ) acc_ipers (Driver.get_children fam) in
          (acc_ipers, acc_ifams)
      | None -> (acc_ipers, acc_ifams)
    ) ipers (ipers, ifams)
  in
  (new_ipers, new_ifams)

(** [apply_genealogical_selection base conf strategy ips] applique la stratégie
    de sélection et retourne les prédicats de filtrage.

    @return
      Triple (sel_per, sel_fam, sets_opt) où sets_opt contient les ensembles
      (ipers, ifams) pour les stratégies généalogiques, ou None pour les autres
*)
let apply_genealogical_selection base conf strategy ips =
  match strategy with
  | SelectAll -> ((fun _ -> true), (fun _ -> true), None)
  | SelectSurnames surnames ->
      let sel_per, sel_fam = select_surnames base surnames in
      (sel_per, sel_fam, None)
  | SelectParentship person_list ->
      let rec loop ipers ifams = function
        | [] ->
            let sel_per i = IPS.mem i ipers in
            let sel_fam i = IFS.mem i ifams in
            (sel_per, sel_fam, Some (ipers, ifams))
        | k2 :: k1 :: tl ->
            let ipers', ifams' = select_parentship base k1 k2 in
            let ipers = IPS.fold IPS.add ipers ipers' in
            let ifams = IFS.fold IFS.add ifams ifams' in
            loop ipers ifams tl
        | [ _ ] ->
            failwith "SelectParentship requires an even number of persons"
      in
      loop IPS.empty IFS.empty person_list
  | SelectAncestors asc ->
      let ipers = select_asc conf base asc ips in
      let per_sel i = IPS.mem i ipers in
      (* Familles où les deux parents sont sélectionnés *)
      let fam_sel i =
        let f = Driver.foi base i in
        per_sel (Driver.get_father f) && per_sel (Driver.get_mother f)
      in
      (* Construire ifams pour la censure *)
      let ifams =
        IPS.fold
          (fun iper acc ->
            Array.fold_left
              (fun acc ifam ->
                if fam_sel ifam && not (IFS.mem ifam acc) then IFS.add ifam acc
                else acc)
              acc
              (Driver.get_family (Driver.poi base iper)))
          ipers IFS.empty
      in
      (per_sel, fam_sel, Some (ipers, ifams))
  | SelectDescendants desc ->
      let ht = Hashtbl.create 0 in
      (* Ajouter seulement les personnes de départ *)
      List.iter (fun i -> Hashtbl.add ht i (Driver.poi base i)) ips;
      (* Ajouter les descendants *)
      let ht_desc =
        let ips_with_depth = List.map (fun i -> (i, 0)) ips in
        Util.select_desc conf base desc ips_with_depth
      in
      Hashtbl.iter (fun i p -> Hashtbl.replace ht i p) ht_desc;
      let sel_per, sel_fam, (ipers, ifams) =
        build_person_and_family_sets base ht
      in
      (sel_per, sel_fam, Some (ipers, ifams))
  | SelectAncDesc { asc; desc } ->
      (* Construire l'ensemble des ascendants *)
      let ht =
        let ips_with_depth = List.map (fun i -> (i, asc)) ips in
        Util.select_mascdesc conf base ips_with_depth desc
      in
      (* Ajouter les descendants *)
      let ht_desc =
        let ips_with_depth = List.map (fun i -> (i, 0)) ips in
        Util.select_desc conf base (-desc) ips_with_depth
      in
      Hashtbl.iter (fun i p -> Hashtbl.replace ht i p) ht_desc;
      let sel_per, sel_fam, (ipers, ifams) =
        build_person_and_family_sets base ht
      in
      (sel_per, sel_fam, Some (ipers, ifams))

(** [apply_censorship_on_subset base opts ipers_opt ifams_opt] applique la
    censure uniquement sur le sous-ensemble sélectionné (plus efficace).

    @param ipers_opt: Option d'ensemble de personnes (None = toute la base)
    @param ifams_opt: Option d'ensemble de familles (None = toute la base)
    @return Paire de prédicats (not_censor_p, not_censor_f) *)
let apply_censorship_on_subset base opts ipers_opt ifams_opt =
  if opts.censor = 0 then ((fun _ -> true), fun _ -> true)
  else
    let threshold =
      if opts.censor = -1 then max_int (* Tout ce qui est vivant *)
      else
        let tm = Unix.localtime (Unix.time ()) in
        1900 + tm.Unix.tm_year - opts.censor
    in

    match (ipers_opt, ifams_opt) with
    | Some ipers, Some ifams ->
        (* Censure sur sous-ensemble : évaluation paresseuse *)
        let not_censor_p i =
          (* Si pas dans l'ensemble sélectionné, pas besoin de vérifier *)
          if not (IPS.mem i ipers) then true
          else not (is_censored_person threshold (Driver.poi base i))
        in

        let not_censor_f i =
          if not (IFS.mem i ifams) then true
          else
            let f = Driver.foi base i in
            not_censor_p (Driver.get_father f)
            && not_censor_p (Driver.get_mother f)
        in

        (not_censor_p, not_censor_f)
    | None, None ->
        (* Censure sur toute la base : utiliser les markers pour efficacité *)
        let pmark = Driver.iper_marker (Driver.ipers base) 0 in
        let fmark = Driver.ifam_marker (Driver.ifams base) 0 in

        if opts.censor = -1 then restrict_base base pmark fmark 1
        else censor_base base pmark fmark 1 threshold;

        (* Marker = 0 signifie NON censuré (à inclure) *)
        ( (fun i -> Collection.Marker.get pmark i = 0),
          fun i -> Collection.Marker.get fmark i = 0 )
    | _ ->
        (* Cas incohérent : ne devrait pas arriver *)
        assert false

(** [select base opts ips] Return filters for [iper] and [ifam] to be used when
    exporting a (portion of a) base.

    @param base The genealogical database
    @param opts
      Export options with filtering criteria:
      - [censor]: Privacy filter (0=none, -1=all living, N>0=born less than N
        years ago)
      - [asc]: Ancestor depth
      - [desc]: Descendant depth
      - [ascdesc]: Legacy option for combined ancestor/descendant depth
        (deprecated, use asc+desc)
      - [surnames]: List of surnames to filter
      - [parentship]: Find relationship paths between person pairs
      - [keys]: Person keys defining the starting set
    @param ips
      Initial list of person indices (currently always empty, keys are used
      instead)
    @return
      Pair of predicates (person_filter, family_filter) where:
      - person_filter i: true if person i should be included
      - family_filter i: true if family i should be included

    The function applies filters in the following order: 1. Build initial person
    set from keys 2. Apply genealogical selection (ancestors, descendants,
    surnames, etc.) 3. Apply censorship filter on the selected subset 4. Combine
    both filters with AND logic

    Note: Censorship is compatible with all selection strategies and is applied
    efficiently only on the selected subset when possible. *)
let select base opts ips =
  (* Étape 1: Construire l'ensemble initial de personnes à partir des clés *)
  let ips =
    List.rev_append ips
    @@ Mutil.filter_map (Gutil.person_of_string_key base) opts.keys
  in
  (* Étape 2: Déterminer et appliquer la stratégie de sélection *)
  let strategy = determine_strategy opts ips in
  let conf = Config.{ empty with wizard = true } in
  let sel_per, sel_fam, sets_opt =
    apply_genealogical_selection base conf strategy ips
  in

  (* Étape 2b: Si -aws, ajouter les siblings *)
  let sets_opt =
    if opts.aws then
      match sets_opt with
      | Some (ipers, ifams) ->
          let ipers, ifams = add_siblings base ipers ifams in
          Some (ipers, ifams)
      | None ->
          (* -aws sur toute la base n'a pas de sens, on ignore *)
          sets_opt
    else sets_opt
  in

  (* Reconstruire les prédicats si les ensembles ont changé *)
  let sel_per, sel_fam = 
    match sets_opt with
    | Some (ipers, ifams) ->
        ((fun i -> IPS.mem i ipers), (fun i -> IFS.mem i ifams))
    | None -> (sel_per, sel_fam)
  in

  (* Étape 3: Appliquer les filtres de censure sur le sous-ensemble *)
  let not_censor_p, not_censor_f =
    match sets_opt with
    | Some (ipers, ifams) ->
        (* Censure efficace sur le sous-ensemble sélectionné *)
        apply_censorship_on_subset base opts (Some ipers) (Some ifams)
    | None ->
        (* Censure sur toute la base (pour SelectAll et SelectSurnames) *)
        apply_censorship_on_subset base opts None None
  in

  (* Étape 4: Combiner les filtres avec ET logique *)
  let person_filter = fun i -> sel_per i && not_censor_p i in
  let family_filter = fun i -> sel_fam i && not_censor_f i in

  (* Étape 5: Vérifier qu'au moins une personne passe les filtres *)
  (match sets_opt with
  | Some (ipers, _) ->
      if not (IPS.exists person_filter ipers) then
        failwith "No persons match the selection criteria"
  | None ->
      (* Pour SelectAll et SelectSurnames sur toute la base : *)
      (* pas de vérification (trop coûteux pour les grandes bases) *)
      ());
  (person_filter, family_filter)

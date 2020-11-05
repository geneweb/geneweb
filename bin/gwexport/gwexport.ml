open Geneweb
open Gwdb

type gwexport_charset = Ansel | Ansi | Ascii | Utf8

type gwexport_opts =
  { asc : int option
  ; ascdesc : int option
  ; base : (string * base) option
  ; censor : int
  ; charset : gwexport_charset
  ; desc : int option
  ; img_base_path : string
  ; keys : string list
  ; mem : bool
  ; no_notes : bool
  ; no_picture : bool
  ; oc : string * out_channel
  ; picture_path : bool
  ; source : string option
  ; surnames : string list
  ; verbose : bool
  }

let opts =
  ref { asc = None
      ; ascdesc = None
      ; base = None
      ; censor = 0
      ; charset = Utf8
      ; desc = None
      ; img_base_path = ""
      ; keys = []
      ; mem = false
      ; no_notes = false
      ; no_picture = false
      ; oc = ("", stderr)
      ; picture_path = false
      ; source = None
      ; surnames = []
      ; verbose = false
      }

let errmsg =
  "Usage: "
  ^ Sys.argv.(0)
  ^ " <base> [options]\n\
     If both options -a and -d are used, intersection is assumed.\n\
     If several options -s are used, union is assumed.\n\
     Options are:"

let print_error s =
  print_endline s ;
  flush stdout ;
  exit 2

let anonfun s =
  if !opts.base = None
  then begin
    Secure.set_base_dir (Filename.dirname s) ;
    opts := { !opts with base = Some (s, Gwdb.open_base s) }
  end else raise (Arg.Bad "Cannot treat several databases")

let speclist =
  let c = opts in
  [ ( "-a", Arg.Int (fun s -> c := { !c with asc = Some s })
    , "<n> maximum generation of the root's ascendants" )
  ; ( "-ad", Arg.Int (fun s -> c := { !c with ascdesc = Some s })
    , "<n> maximum generation of the root's ascendants descendants" )
  ; ( "-key", Arg.String (fun s -> c := { !c with keys = s :: !c.keys })
    , " key reference of root person. Used for -a/-d options. Can be used multiple times." )
  ; ( "-c", Arg.Int (fun s -> c := { !c with censor = s }),
      "<num>: when a person is born less than <num> years ago, it is not exported \
       unless it is Public. All the spouses and descendants are also censored." )
  ; ( "-charset"
    , Arg.String begin fun s -> c := { !c with charset = match s with
          | "ASCII" -> Ascii
          | "ANSEL" -> Ansel
          | "ANSI" -> Ansi
          | "UTF-8" -> Utf8
          | _ -> raise (Arg.Bad "bad -charset value")
        }
      end
    , "[ASCII|ANSEL|ANSI|UTF-8] set charset; default is UTF-8" )
  ; ( "-d", Arg.Int (fun s -> c := { !c with desc = Some s })
    , "<n> maximum generation of the root's descendants" )
  ; ( "-mem", Arg.Unit (fun () -> c := { !c with mem = true })
    , " save memory space, but slower" )
  ; ( "-nn", Arg.Unit (fun () -> c := { !c with no_notes = true })
    , " no (database) notes" )
  ; ( "-nopicture", Arg.Unit (fun () -> c := { !c with no_picture = true })
    , " don't extract individual picture" )
  ; ( "-o", Arg.String (fun s -> c := { !c with oc = (s, open_out s) })
    , "<ged> output file name (default: stdout)" )
  ; ( "-picture-path", Arg.Unit (fun () -> c := { !c with picture_path = true })
    , " extract pictures path" )
  ; ( "-s", Arg.String (fun x -> c := { !c with surnames = x :: !c.surnames })
    , "<sn> select this surname (option usable several times)" )
  ; ( "-source", Arg.String (fun x -> c := { !c with source = Some x })
    , "<src> replace individuals and families sources. Also delete event sources." )
  ; ( "-v", Arg.Unit (fun () -> c := { !c with verbose = true })
    , " verbose" )
  ]


module IPS = Util.IperSet
module IFS = Util.IfamSet

(**/**)
let is_censored_person threshold p =
  match Adef.od_of_cdate (get_birth p) with
  | None -> false
  | Some date ->
    match date with
    | Dgreg (dmy, _) -> dmy.year >= threshold && get_access p != Public
    | _ -> false

let is_censored_couple base threshold cpl =
  is_censored_person threshold @@ poi base (get_father cpl)
  || is_censored_person threshold @@ poi base (get_mother cpl)

let censor_person base pmark flag threshold p no_check =
  let ps = poi base p in
  if no_check || is_censored_person threshold ps
  then Marker.set pmark p (Marker.get pmark p lor flag)

let rec censor_family base pmark fmark flag threshold i no_check =
  let censor_unions p =
    let uni = poi base p in
    Array.iter begin fun ifam ->
      censor_family base pmark fmark flag threshold ifam true ;
      censor_person base pmark flag threshold p true
    end (get_family uni)
  in
  let censor_descendants f =
    let des = foi base f in
    Array.iter begin fun iper ->
      if Marker.get pmark iper = 0 then censor_unions iper
    end (get_children des)
  in
  let all_families_censored p =
    (* FIXME: replace with forall *)
    let uni = poi base p in
    Array.fold_left begin fun check ifam ->
      check && Marker.get fmark ifam = 0
    end true (get_family uni)
  in
  let censor_spouse iper =
    if all_families_censored iper
    then Marker.set pmark iper (Marker.get pmark iper lor flag)
  in
  if Marker.get fmark i = 0 then
    let fam = foi base i in
    if no_check || is_censored_couple base threshold fam then begin
      Marker.set fmark i (Marker.get fmark i lor flag) ;
      censor_spouse (get_father fam) ;
      censor_spouse (get_mother fam) ;
      censor_descendants i
    end

let censor_base base pmark fmark flag threshold =
  Collection.iter begin fun i ->
    censor_family base pmark fmark flag threshold i false
  end (ifams base) ;
  Collection.iter begin fun i ->
    censor_person base pmark flag threshold i false
  end (ipers base)

let restrict_base base per_tab fam_tab flag =
  Collection.iter begin fun i ->
    if base_visible_get base (fun _ -> false) i
    then Marker.set per_tab i (Marker.get per_tab i lor flag)
  end (ipers base) ;
  Collection.iter begin fun i ->
    let fam = foi base i in
    let des_visible =
      (* FIXME: replace with exists *)
      Array.fold_left
        (fun check iper -> check || Marker.get per_tab iper = 0) false
        (get_children fam)
    in
    let cpl_not_visible =
      Marker.get per_tab (get_father fam) <> 0
      || Marker.get per_tab (get_mother fam) <> 0
    in
    if not des_visible && cpl_not_visible
    then Marker.set fam_tab i (Marker.get fam_tab i lor flag)
  end (ifams base)

let select_asc conf base max_gen ips =
  let rec loop_asc (gen : int) set ip =
    if not @@ IPS.mem ip set then begin
      let set = IPS.add ip set in
      let p = poi base ip in
      if gen < max_gen then match get_parents p with
        | Some ifam ->
          let cpl = foi base ifam in
          let set = loop_asc (gen + 1) set (get_father cpl) in
          loop_asc (gen + 1) set (get_mother cpl)
        | _ -> set
      else set
    end else set
  in
  List.fold_left (loop_asc 0) IPS.empty ips

(* Should it use search engine functions? *)
let select_surname base pmark fmark surname =
  let surname = Name.strip_lower surname in
  Collection.iter begin fun i ->
    let fam = foi base i in
    let fath = poi base (get_father fam) in
    let moth = poi base (get_mother fam) in
    if Name.strip_lower (sou base (get_surname fath)) = surname
    || Name.strip_lower (sou base (get_surname moth)) = surname
    then begin
      Marker.set fmark i true ;
      Marker.set pmark (get_father fam) true ;
      Marker.set pmark (get_mother fam) true ;
      Array.iter
        (fun ic ->
           let p = poi base ic in
           if not (Marker.get pmark ic)
           && Name.strip_lower (sou base (get_surname p)) = surname
           then Marker.set pmark ic true)
        (get_children fam);
    end
  end (ifams base)

let select_surnames base surnames : (iper -> bool) * (ifam -> bool) =
  let pmark = Gwdb.iper_marker (Gwdb.ipers base) false in
  let fmark = Gwdb.ifam_marker (Gwdb.ifams base) false in
  List.iter (select_surname base pmark fmark) surnames ;
  ( (fun i -> Gwdb.Marker.get pmark i)
  , (fun i -> Gwdb.Marker.get fmark i) )
(**/**)

(** [select ?asc ?ascdesc ?desc ?censor ?surnames ?keys base]
    Return filters for [iper] and [ifam] to be used when exporting
    a (portion of a) base.
*)
let select opts ips =
  match opts.base with
  | None -> print_error "Missing base name\nUse option -help for usage\n";
  | Some (_, base) ->
    let ips = List.rev_append ips @@ Util.filter_map (Gutil.person_of_string_key base) opts.keys in
    let not_censor_p, not_censor_f =
      if opts.censor <> 0
      then begin
        let pmark = iper_marker (ipers base) 0 in
        let fmark = ifam_marker (ifams base) 0 in
        if opts.censor = -1 then restrict_base base pmark fmark 1
        else begin
          let tm = Unix.localtime (Unix.time ()) in
          let threshold = 1900 + tm.Unix.tm_year - opts.censor in
          censor_base base pmark fmark 1 threshold
        end ;
        ( (fun i -> Marker.get pmark i = 0)
        , (fun i -> Marker.get fmark i = 0)
        )
      end
      else (fun _ -> true), (fun _ -> true)
    in
    let conf = { Config.empty with wizard = true } in
    let sel_per, sel_fam =
      if (opts.asc <> None && opts.ascdesc <> None) || opts.desc <> None then begin
        assert (opts.censor = 0) ;
        let asc = Opt.default max_int opts.asc in
        let desc = - (Opt.default max_int opts.desc) in
        let ht =
          match opts.ascdesc with
          | Some ascdesc ->
            let ips = List.map (fun i -> (i, asc)) ips in
            Util.select_mascdesc conf base ips ascdesc
          | None ->
            let ht = Hashtbl.create 0 in
            IPS.iter (fun i -> Hashtbl.add ht i (poi base i)) (select_asc conf base asc ips) ;
            ht
        in
        let ht' =
          let ips = List.map (fun i -> (i, 0)) ips in
          Util.select_desc conf base desc ips
        in
        Hashtbl.iter (fun i p -> Hashtbl.replace ht i p) ht' ;
        let ipers =
          Hashtbl.fold begin fun i _ ipers -> IPS.add i ipers end ht IPS.empty
        in
        let ifams =
          IPS.fold begin fun iper acc ->
            Array.fold_left begin fun acc ifam ->
              if IFS.mem ifam acc
              || not (IPS.mem (Gutil.spouse iper @@ foi base ifam) ipers)
              then acc
              else IFS.add ifam acc
            end acc (get_family (poi base iper))
          end ipers IFS.empty
        in
        let sel_per i = IPS.mem i ipers in
        let sel_fam i = IFS.mem i ifams in
        (sel_per, sel_fam)
      end else match opts.asc with
        | Some asc ->
          let ipers = select_asc conf base asc ips in
          let per_sel i = IPS.mem i ipers in
          let fam_sel i =
            let f = foi base i in
            per_sel (get_father f) && per_sel (get_mother f)
          in
          (per_sel, fam_sel)
        | None ->
          if opts.surnames = [] then (fun _ -> true), (fun _ -> true)
          else select_surnames base opts.surnames
    in
    ( (fun i -> not_censor_p i && sel_per i)
    , (fun i -> not_censor_f i && sel_fam i)
    )

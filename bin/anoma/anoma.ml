(** anoma.ml — Roglo GeneWeb anomaly report generator.
    Replaces anoma.pl: Driver replaces curl; Name.lower replaces good_iso/bad_iso.
    Base opened via Driver.with_database. *)

open Printf
module Driver = Geneweb_db.Driver

(* ===== Configuration ===== *)

let base_cop   = "/data/roglo/base-bis"   (* base-ter on off-weeks; forced to base-bis *)
let wizst      = "/data/wizmagic/stat/"
let note_adm   = "/home/roglo/base/wizard.gwb/base_d/notes_d/Admin"
let base_nor_d = "/home/roglo/base/wizard.gwb/base_d/notes_d/Roglo"
let admin_or   = "/data/bugs"
let base_dev   = "/data/roglo/basenew"

let bname      = ref "roglo"
let bdir       = ref (Secure.base_dir ())

let warning_log () = base_dev ^ "/sources/" ^ !bname ^ "-wrn.log"

(* ===== Time helpers ===== *)

let start_t = Unix.time ()
let now     = Unix.localtime start_t
let wday    = now.Unix.tm_wday

let fr_months =
  [|"?"; "janvier"; "février"; "mars"; "avril"; "mai"; "juin";
    "juillet"; "août"; "septembre"; "octobre"; "novembre"; "décembre"|]

let dateu path =
  let t =
    try Unix.localtime (Unix.stat path).Unix.st_mtime
    with Unix.Unix_error _ -> now
  in
  sprintf "le %02d %s à %02d:%02d"
    t.Unix.tm_mday fr_months.(t.Unix.tm_mon + 1)
    t.Unix.tm_hour t.Unix.tm_min

(* ===== IO helpers ===== *)

let read_lines path =
  let ic = open_in path in
  let acc = ref [] in
  (try while true do acc := input_line ic :: !acc done
   with End_of_file -> ());
  close_in ic;
  List.rev !acc

let with_out path f =
  let oc = open_out path in
  (try f oc with e -> close_out oc; raise e);
  close_out oc

let with_out_append path f =
  let oc = open_out_gen [Open_append; Open_creat; Open_text] 0o644 path in
  (try f oc with e -> close_out oc; raise e);
  close_out oc

(* ===== Database helpers (base passed explicitly) ===== *)

(** Look up a person by (first_name, surname, occurrence-as-string).
    Name.lower handles the same normalisation as good_iso/bad_iso. *)
let find_person base p1 n1 i1 =
  let oc = try int_of_string (String.trim i1) with Failure _ -> 0 in
  match Driver.person_of_key base (Name.lower p1) (Name.lower n1) oc with
  | Some ip -> Some (Driver.poi base ip)
  | None    -> None

(** First #-prefixed line of a person's notes — replaces the templ=src curl call. *)
let person_src_snippet base p1 n1 i1 =
  match find_person base p1 n1 i1 with
  | None   -> ""
  | Some p ->
    let notes = Driver.sou base (Driver.get_notes p) in
    (match String.split_on_char '\n' notes with
     | hd :: _ when String.length hd > 0 && hd.[0] = '#' -> "\n" ^ hd
     | _ -> "")

(** Full display name — replaces the templ=rgpd curl call. *)
let person_long_name base p1 n1 i1 =
  match find_person base p1 n1 i1 with
  | None   -> "######"
  | Some p ->
    let fn = Driver.sou base (Driver.get_first_name p) in
    let sn = Driver.sou base (Driver.get_surname p) in
    fn ^ " " ^ sn

(* ===== Normal (whitelist) ===== *)

let lien_re =
  Str.regexp {|\[\[\([^]/]*/[^]/]*/[0-9]+\)[^]]*\]\]\(.*\)|}

(** Load Normal.txt → (normal, nomlon) hashtables.
    normal : lien → comment
    nomlon : lien → long display name (fetched from base) *)
let load_normal base =
  let normal : (string, string) Hashtbl.t = Hashtbl.create 512 in
  let nomlon : (string, string) Hashtbl.t = Hashtbl.create 512 in
  let path  = admin_or ^ "/Normal.txt" in
  let lines = try read_lines path with Sys_error _ -> [] in
  List.iter (fun line ->
    if String.length line > 0 && line.[0] = '*' then
      if Str.string_match lien_re line 0 then begin
        let raw  = Str.matched_group 1 line in
        let lien = String.lowercase_ascii
                     (Str.global_replace (Str.regexp {| */ *|}) "/" raw) in
        let comm = let c = String.trim
                              (try Str.matched_group 2 line with Not_found -> "") in
                   if c = "" then "-" else c in
        Hashtbl.replace normal lien comm;
        (match String.split_on_char '/' lien with
         | [p1; n1; i1] ->
           Hashtbl.replace nomlon lien (person_long_name base p1 n1 i1)
         | _ -> ())
      end
  ) lines;
  (normal, nomlon)

let write_normaux normal nomlon =
  if Hashtbl.length nomlon = 0 then ()
  else begin
    let pairs =
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) normal []
      |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    in
    with_out (admin_or ^ "/Normaux.txt") (fun oc ->
      List.iter (fun (lien, comm) ->
        let long = try Hashtbl.find nomlon lien with Not_found -> "######" in
        fprintf oc "*[[%s/%s]]%s\n" lien long comm
      ) pairs
    )
  end

(* ===== Log pre-processing ===== *)

(** Read warning log, normalise to one-warning-per-line, dedup, write roglo1.log.
    Mirrors: s/\n/#/; s/Warning: /\n*/; | sort -u *)
let preprocess_log () =
  let raw    = try read_lines (warning_log ()) with Sys_error _ -> [] in
  let joined = String.concat "#" raw in
  let lines  =
    String.split_on_char '\n' joined
    |> List.concat_map (fun chunk ->
         Str.global_replace (Str.regexp {|Warning: |}) "\n*" chunk
         |> String.split_on_char '\n')
    |> List.filter (fun s -> s <> "")
    |> List.sort_uniq String.compare
  in
  with_out (base_cop ^ "/roglo1.log") (fun oc ->
    List.iter (fun l -> output_string oc (l ^ "\n")) lines
  );
  lines

(* ===== Filtering ===== *)

let search_int re line =
  try
    ignore (Str.search_forward (Str.regexp re) line 0);
    Some (int_of_string (String.trim (Str.matched_group 1 line)))
  with Not_found | Failure _ -> None

let has_str sub line =
  try ignore (Str.search_forward (Str.regexp_string sub) line 0); true
  with Not_found -> false

let should_skip line =
  has_str "in order" line
  || has_str "order of" line
  || has_str "Changed order of children" line
  || (match search_int {|is \([0-9]+\) years old|} line with
      | Some a -> a < 100 | None -> false)
  || (match search_int {|was parent at age of \([0-9]+\)|} line with
      | Some a -> a < 63 && a > 12 | None -> false)
  || (match search_int {|at the advanced age of \([0-9]+\) years|} line with
      | Some a -> a < 100 | None -> false)
  || (match search_int {|is quite important: \([0-9]+\)|} line with
      | Some a -> a < 60 | None -> false)
  || (match search_int {|married at age \([0-9]+\)|} line with
      | Some a -> a = 12 && wday > 0 | None -> false)

(* ===== Translation: English → French ===== *)

let sub re repl s = Str.global_replace (Str.regexp re) repl s

let sub_age re fr_fmt s =
  Str.global_substitute (Str.regexp re)
    (fun s ->
       let n = try Str.matched_group 1 s with Not_found -> "0" in
       sprintf fr_fmt n)
    s

let translate line =
  line
  |> sub {|Warning: |}                          "*"
  |> sub {|Rgpd status: .*|}                    ""
  |> sub {|Starting gwc2.*|}                    ""
  |> sub {|"|}                                  ""
  |> (fun s ->
      Str.global_substitute
        (Str.regexp {|The difference of age between \(.*\) and \(.*\) is quite |})
        (fun s ->
           let a = try Str.matched_group 1 s with Not_found -> "?" in
           let b = try Str.matched_group 2 s with Not_found -> "?" in
           sprintf "L'écart entre#%s#et#%s#est " a b)
        s)
  |> sub {|The following children of|}          "L'écart entre les enfants de"
  |> sub {|are born very close|}                "est trop court"
  |> sub {| his/her child |}                    " son enfant#"
  |> sub {|married before|}                     "s'est marié avant#"
  |> sub {|married after |}                     "s'est marié après#"
  |> sub {|was witness before|}                 "était témoin avant#"
  |> sub {|was witness after |}                 "était témoin après#"
  |> sub {|marriage before|}                    "s'est marié avant#"
  |> sub {|marriage after |}                    "s'est marié après#"
  |> sub {|Changed order of marriages of|}      "Ordre des mariages modifié pour :#"
  |> sub {|baptised before|}                    "a été baptisé avant#"
  |> sub_age {| *was parent at age of \([0-9]+\)|}
                                                "# a été parent à l'âge de %s ans"
  |> sub_age {|at age of \([0-9]+\)|}          "à l'âge de %s ans"
  |> sub_age {| *is \([0-9]+\) years old|}     "#est âgé de %s ans"
  |> sub_age {| *died at the advanced age of \([0-9]+\) years old|}
                                                "# est âgé de %s ans à son décès"
  |> sub_age {| married at age \([0-9]+\)|}    "#est âgé de %s ans à son mariage"
  |> sub {|is born |}                           "est né "
  |> sub {|born after |}                        "#est né après "
  |> sub {|has a younger ancestor|}             "a un ancêtre plus jeune"
  |> sub {| more than 2 years|}                " plus de 2 ans"
  |> sub {|after the death of his/her father|}  "après la mort de son père"
  |> sub {|after the death of his/her mother|}  "après la mort de sa mère"
  |> sub {|his/her birth|}                      "sa naissance"
  |> sub {|his/her death|}                      "sa mort"
  |> sub {|sex not coherent with relations|}    "n'est pas du sexe indiqué dans ses relations"
  |> sub {|#and#|}                              "#et#"
  |> sub {|# *|}                               "#"

(* ===== Person key extraction ===== *)

let re_person = Str.regexp {|\([^*#/]+\)\.\([0-9]+\) \([^*#/]+\)|}

type persons =
  | None_found
  | One of string * string * string
  | Two of string * string * string * string * string * string

let find_persons line =
  try
    ignore (Str.search_forward re_person line 0);
    let p1   = String.trim (Str.matched_group 1 line) in
    let i1   = String.trim (Str.matched_group 2 line) in
    let n1   = String.trim (Str.matched_group 3 line) in
    let end1 = Str.match_end () in
    (try
       ignore (Str.search_forward re_person line end1);
       let p2 = String.trim (Str.matched_group 1 line) in
       let i2 = String.trim (Str.matched_group 2 line) in
       let n2 = String.trim (Str.matched_group 3 line) in
       Two (p1, i1, n1, p2, i2, n2)
     with Not_found -> One (p1, i1, n1))
  with Not_found -> None_found

(* ===== Link construction ===== *)

let lien_key p n i = String.lowercase_ascii (sprintf "%s/%s/%s" p n i)

let insert_link1 line p i n =
  let old = Str.regexp_string (sprintf "%s.%s %s" p i n) in
  Str.global_replace old (sprintf "[[%s/%s/%s/%s %s]]" p n i p n) line

let insert_link2_href line p1 n1 i1 p2 i2 n2 =
  let old  = Str.regexp_string (sprintf " %s.%s %s" p2 i2 n2) in
  let href = sprintf
    {| <a href="%%sm=A;t=D;p=%s;n=%s;oc=%s;p1=%s;n1=%s;oc1=%s;l=9;spouse=on">%s.%s %s</a>|}
    p1 n1 i1 p2 n2 i2 p2 i2 n2 in
  Str.global_replace old href line

let insert_link2_wiki line p2 i2 n2 =
  let old  = Str.regexp_string (sprintf " %s.%s %s" p2 i2 n2) in
  let wiki = sprintf " [[%s/%s/%s/%s %s]]" p2 n2 i2 p2 n2 in
  Str.global_replace old wiki line

let hashes_to_spaces s = Str.global_replace (Str.regexp "#") " " s

(* ===== grep-count helper ===== *)

let count_in_log1 pat =
  let lines = try read_lines (base_cop ^ "/roglo1.log") with Sys_error _ -> [] in
  List.length (List.filter (fun l ->
    try ignore (Str.search_forward (Str.regexp pat) l 0); true
    with Not_found -> false
  ) lines)

(* ===== Core computation (receives open base) ===== *)

let compute base =
  eprintf "\n#######Lecture Normales#######\n%!";
  let (normal, nomlon) = load_normal base in
  write_normaux normal nomlon;

  eprintf "\n#######Lecture Warnings#######\n%!";
  let lines = preprocess_log () in

  let anoma = Array.make 4 ""    in
  let paren = Array.make 9000 "" in
  let vieux = Array.make 9000 "" in

  let nbanoma  = ref 0 in
  let nbecart  = ref 0 in
  let nbnormal = ref 0 in
  let nbpar    = ref 0 in

  let oc_bet = open_out (wizst ^ "roglo2.log") in
  let oc_nor = open_out (wizst ^ "roglo3.log") in

  List.iter (fun raw ->
    if Str.string_match (Str.regexp {|^#? *$|}) raw 0 then ()
    else if should_skip raw then ()
    else begin
      let line = translate raw in
      match find_persons line with

      | Two (p1, i1, n1, p2, i2, n2) ->
        let lk1  = lien_key p1 n1 i1 in
        let line = insert_link1 line p1 i1 n1 in
        if Hashtbl.mem normal lk1 then begin
          incr nbnormal;
          output_string oc_nor (hashes_to_spaces line ^ "\n")
        end else begin
          let src  = person_src_snippet base p1 n1 i1 in
          let line = if src <> "" then line ^ src else line in
          if has_str "entre les enfants" line || has_str "cart entre" line then begin
            let line = insert_link2_wiki line p2 i2 n2 in
            incr nbecart;
            output_string oc_bet (hashes_to_spaces line ^ "\n")
          end else begin
            let line = insert_link2_href line p1 n1 i1 p2 i2 n2 in
            incr nbanoma;
            anoma.(2) <- anoma.(2) ^ hashes_to_spaces line ^ "\n"
          end
        end

      | One (p1, i1, n1) ->
        let lk1     = lien_key p1 n1 i1 in
        let line    = insert_link1 line p1 i1 n1 in
        let line_sp = hashes_to_spaces line in
        (* Centenarian list — $wday*0==0 is always true in original, kept as-is *)
        (match search_int {|\([0-9]+\) ans à son décès|} line_sp with
         | Some age when age >= 100 ->
           let src = person_src_snippet base p1 n1 i1 in
           vieux.(age) <- vieux.(age) ^ line_sp ^ "\n" ^ src
         | _ -> ());
        if Hashtbl.mem normal lk1 then begin
          incr nbnormal;
          output_string oc_nor (line_sp ^ "\n")
        end else begin
          match search_int {|\([0-9]+\) ans à son décès|} line_sp with
          | Some age when age < 118 -> ()
          | Some _ ->
            let src = person_src_snippet base p1 n1 i1 in
            incr nbanoma;
            anoma.(3) <- anoma.(3) ^ line_sp ^ "\n" ^ src
          | None ->
            match search_int {|\([0-9]+\) ans à son mariage|} line_sp with
            | Some age ->
              let src = person_src_snippet base p1 n1 i1 in
              incr nbpar;
              paren.(age) <- paren.(age) ^ line_sp ^ "\n" ^ src
            | None ->
              let src = person_src_snippet base p1 n1 i1 in
              let out = if src <> "" then line_sp ^ "\n" ^ src else line_sp in
              eprintf "%2d 1 %s%!" !nbanoma line_sp;
              incr nbanoma;
              anoma.(1) <- anoma.(1) ^ out ^ "\n"
        end

      | None_found ->
        let line_sp = hashes_to_spaces line in
        if String.trim line_sp <> "" then begin
          eprintf "%2d 0 {%s}%!" !nbanoma line_sp;
          incr nbanoma;
          anoma.(0) <- anoma.(0) ^ line_sp ^ "\n"
        end
    end
  ) lines;

  close_out oc_bet;
  close_out oc_nor;

  let date_w = dateu (warning_log ()) in

  with_out (note_adm ^ "/anoma.txt") (fun oc ->
    fprintf oc "== Anomalies relevées %s==\n\n" date_w;
    fprintf oc "=== Informations sur les extrêmes ===\n";
    fprintf oc "*Personnes mariées à 11 ans : %d\n"
      (count_in_log1 "married at age 11");
    fprintf oc "*[[[Roglo:Epoux/Personnes mariées à 12 ans]]] : %d\n"
      (count_in_log1 "married at age 12");
    fprintf oc "*Décès à 10x ans : %d ([[[Admin:Vieux/Liste des centenaires]]])\n"
      (count_in_log1 " the advanced age of 10. years old");
    fprintf oc "*Décès à 11x ans : %d\n"
      (count_in_log1 " the advanced age of 11. years old");
    fprintf oc "*Décès à 12x ans : %d\n"
      (count_in_log1 " the advanced age of 12. years old");
    fprintf oc "\n\nCompléments sur les [[[Roglo:erreurs/anomalies]]]\n";
    Array.iter (output_string oc) anoma;
    let elapsed = int_of_float (Unix.time () -. start_t) in
    fprintf oc
      "*%d anomalies et %d écarts détectés, %d anomalies ignorées, 0 marié(e)s, %d parents\n"
      !nbanoma !nbecart !nbnormal !nbpar;
    fprintf oc "*Traitement des Anomalies en %02d:%02d minutes %02d secondes\n"
      (elapsed / 3600) ((elapsed mod 3600) / 60) (elapsed mod 60)
  );

  with_out (note_adm ^ "/Vieux.txt") (fun oc ->
    fprintf oc "=== Décès des centenaires relevés %s===\n\n" date_w;
    for i = 100 to 8999 do
      if vieux.(i) <> "" then fprintf oc "\n * %d ans *\n%s" i vieux.(i)
    done
  );

  with_out_append (note_adm ^ "/anoma.txt") (fun oc ->
    fprintf oc "=== Jeunes et vieux parents ===\n";
    for i = 0 to 8999 do
      if paren.(i) <> "" then fprintf oc "\n * %d ans *\n%s" i paren.(i)
    done;
    fprintf oc "== Écarts relevés %s==\n\n" date_w
  );

  (try
     let ecart = read_lines (wizst ^ "roglo2.log") in
     with_out_append (note_adm ^ "/anoma.txt") (fun oc ->
       List.iter (fun l -> output_string oc (l ^ "\n")) ecart
     )
   with Sys_error _ -> ());

  ignore (Sys.command (sprintf "/home/roglo/SH/gdu >%s/quotas.txt" note_adm));

  printf
    "%d anomalies et %d écarts détectés, %d anomalies ignorées, 0 marié(e)s, %d parents\n"
    !nbanoma !nbecart !nbnormal !nbpar;
  printf "Anomalies : Fin - Temps: %4d\n"
    (int_of_float (Unix.time () -. start_t))

(* ===== Entry point ===== *)

let () =
  let speclist =
    [ "-b",  Arg.Set_string bname, "Database name (default: roglo)"
    ; "-bd", Arg.Set_string bdir,  "Bases directory (default: Secure.base_dir)" ]
  in
  Arg.parse speclist (fun s -> bname := s) "Usage: anoma [-b bname] [-bd dir]";

  let t0 = Unix.localtime (Unix.time ()) in
  printf "%02d/%02d/%04d %02d:%02d:%02d\n"
    t0.Unix.tm_mday (t0.Unix.tm_mon + 1) (t0.Unix.tm_year + 1900)
    t0.Unix.tm_hour t0.Unix.tm_min t0.Unix.tm_sec;

  let baseadm = "/data/roglo/base-ter" in
  ignore (Sys.command (sprintf "cp %s/admin.gwf %s/roglo.gwf"  baseadm baseadm));
  ignore (Sys.command (sprintf "cp %s/admin.gwf %s/wizard.gwf" baseadm baseadm));

  Driver.with_database (Filename.concat !bdir !bname) (fun base -> compute base);

  let t1 = Unix.localtime (Unix.time ()) in
  printf "%02d/%02d/%04d %02d:%02d:%02d\n"
    t1.Unix.tm_mday (t1.Unix.tm_mon + 1) (t1.Unix.tm_year + 1900)
    t1.Unix.tm_hour t1.Unix.tm_min t1.Unix.tm_sec

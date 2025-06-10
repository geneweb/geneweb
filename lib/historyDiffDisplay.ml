(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
open HistoryDiff
module Ast = Geneweb_templ.Ast
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let escape_html s = (Util.escape_html s :> Adef.safe_string)

let print_clean conf =
  match p_getenv conf.env "f" with
  | Some f when f <> "" ->
      let title _ =
        transl conf "clean history"
        |> Utf8.capitalize_fst |> Output.print_sstring conf
      in
      Hutil.header conf title;
      Util.gen_print_tips conf
        ("select the input you want to erase from the history" |> transl conf
       |> Utf8.capitalize_fst |> Adef.safe);
      let history = load_person_history conf f in
      Output.print_sstring conf {|<form method="post" action="|};
      Output.print_sstring conf conf.command;
      Output.print_sstring conf "\">";
      Util.hidden_input conf "m" ("HIST_CLEAN_OK" |> Adef.encoded);
      Util.hidden_input conf "f" (Mutil.encode f);
      Output.print_sstring conf "<ul>";
      let rec loop i = function
        | [] -> ()
        | gr :: l ->
            Output.print_sstring conf "<li><label>";
            Output.print_sstring conf {|<input type="checkbox" name="i|};
            Output.print_sstring conf (string_of_int i);
            Output.print_sstring conf {|" value="on">|};
            Output.print_string conf gr.date;
            Output.print_sstring conf {| |};
            Output.print_string conf gr.HistoryDiff.wizard;
            Output.print_sstring conf "</label></li>";
            loop (i + 1) l
      in
      loop 0 history;
      Output.print_sstring conf
        {|</ul><button type="submit" class="btn btn-primary btn-lg">|};
      transl_nth conf "validate/delete" 0
      |> Utf8.capitalize_fst |> Output.print_sstring conf;
      Output.print_sstring conf "</button></form>";
      Hutil.trailer conf
  | _ -> Hutil.incorrect_request conf ~comment:"no history file provided"

(* ************************************************************************ *)
(*  [Fonc] print_clean_ok : config -> unit                                  *)

(* ************************************************************************ *)

(** [Description] : Ré-écrit le fichier historique lié à une personne en ayant
    supprimé les entrées non désirées. [Args] :
    - conf : configuration de la base [Retour] : Néant [Rem] : Exporté en clair
      hors de ce module. *)
let print_clean_ok conf =
  let rec clean_history i history new_history =
    match history with
    | [] -> new_history
    | gr :: l ->
        let lab = "i" ^ string_of_int i in
        if p_getenv conf.env lab = Some "on" then
          clean_history (i + 1) l new_history
        else clean_history (i + 1) l (gr :: new_history)
  in
  match p_getenv conf.env "f" with
  | Some f when f <> "" ->
      let title _ =
        transl conf "history cleaned"
        |> Utf8.capitalize_fst |> Output.print_sstring conf
      in
      Hutil.header conf title;
      let history = load_person_history conf f in
      let new_history = clean_history 0 history [] in
      let fname = history_path conf f in
      (if new_history = [] then Mutil.rm fname
       else
         let ext_flags =
           [ Open_wronly; Open_trunc; Open_creat; Open_binary; Open_nonblock ]
         in
         match
           try Some (Secure.open_out_gen ext_flags 0o644 fname)
           with Sys_error _ -> None
         with
         | Some oc ->
             List.iter (fun v -> output_value oc (v : gen_record)) new_history;
             close_out oc
         | None -> ());
      Hutil.trailer conf
  | _ -> Hutil.incorrect_request conf

(**/**) (* Template *)

let person_of_gen_p_key base gen_p =
  match Driver.person_of_key base gen_p.first_name gen_p.surname gen_p.occ with
  | Some ip -> Driver.poi base ip
  | None -> Driver.empty_person base Driver.Iper.dummy

(* N'est pas forcément très précis. En effet, on enregistre que     *)
(* les ipers. Or lors d'un nettoyage de la base, il se peut que     *)
(* ces ipers changent. On peut donc pointer vers une autre persone. *)
let person_of_iper conf base ip =
  try
    let p = pget conf base ip in
    if authorized_age conf base p then gen_person_text conf base p
    else Adef.safe ""
  with _ -> Adef.safe ""

let person_of_iper_array conf base ipl =
  (Array.fold_right
     (fun ip acc ->
       let x = person_of_iper conf base ip in
       if (x :> string) = "" then acc else x :: acc)
     ipl []
    : Adef.safe_string list
    :> string list)
  |> String.concat ", " |> Adef.safe

let string_of_cdate conf cod =
  match Date.od_of_cdate cod with
  | Some d -> DateDisplay.string_slash_of_date conf d
  | None -> Adef.safe ""

let string_of_death conf death =
  match Date.date_of_death death with
  | Some cd -> DateDisplay.string_slash_of_date conf cd
  | None -> Adef.safe ""

let string_of_burial conf burial =
  match burial with
  | Buried cod | Cremated cod -> string_of_cdate conf cod
  | UnknownBurial -> Adef.safe ""

let string_of_title conf titles : Adef.safe_string =
  let string_of_t_name t =
    match t.t_name with Tname s -> escape_html s | _ -> Adef.safe ""
  in
  let one_title t =
    let name = escape_html (t.t_ident ^ " " ^ t.t_place) in
    let name = if (name :> string) = " " then Adef.safe "" else name in
    let dates =
      string_of_cdate conf t.t_date_start
      ^^^ "-"
      ^<^ string_of_cdate conf t.t_date_end
    in
    let dates =
      if (dates :> string) = "-" then Adef.safe "" else "(" ^<^ dates ^>^ ")"
    in
    let nth =
      let t_name = string_of_t_name t in
      if (t_name :> string) = "" then
        Adef.safe (if t.t_nth = 0 then "" else string_of_int t.t_nth)
      else t_name ^>^ " " ^ string_of_int t.t_nth
    in
    let nth =
      if (nth :> string) = "" then Adef.safe "" else "[" ^<^ nth ^>^ "]"
    in
    name
    ^^^ (if (name :> string) = "" then "" else " ")
    ^<^ nth
    ^^^ (if (nth :> string) = "" then "" else " ")
    ^<^ dates
  in
  List.fold_left
    (fun (acc : Adef.safe_string) t ->
      if (acc :> string) = "" then one_title t else acc ^^^ ", " ^<^ one_title t)
    (Adef.safe "") titles

let string_of_related conf base ip related : Adef.safe_string =
  List.fold_right
    (fun ic acc ->
      let p = person_of_iper conf base ip in
      if (p :> string) = "" then acc
      else
        let c = try pget conf base ic with _ -> Driver.empty_person base ic in
        let rel =
          let rec loop rp =
            match rp with
            | [] -> Adef.safe ""
            | r :: l -> (
                match r.r_fath with
                | Some ifath when ifath = ip ->
                    Util.rchild_type_text conf r.r_type 2
                | _ -> loop l)
          in
          loop (Driver.get_rparents c)
        in
        (Utf8.capitalize_fst (rel : Adef.safe_string :> string)
        ^<^ transl conf ":" ^<^ p)
        :: acc)
    related []
  |> (fun s -> String.concat ", " (s :> string list))
  |> Adef.safe

let string_of_rparents conf base rparents : Adef.safe_string =
  List.fold_right
    (fun rp accu ->
      match (rp.r_fath, rp.r_moth) with
      | Some ip1, Some ip2 -> (
          let rel =
            (Util.relation_type_text conf rp.r_type 2
              : Adef.safe_string
              :> string)
            |> Utf8.capitalize_fst
          in
          let fath = person_of_iper conf base ip1 in
          let moth = person_of_iper conf base ip2 in
          match ((fath :> string), (moth :> string)) with
          | "", "" -> accu
          | _, "" -> (rel ^<^ transl conf ":" ^<^ fath) :: accu
          | "", _ -> (rel ^<^ transl conf ":" ^<^ moth) :: accu
          | _, _ -> (rel ^<^ transl conf ":" ^<^ fath ^^^ ", " ^<^ moth) :: accu
          )
      | Some ip, None ->
          let p = person_of_iper conf base ip in
          if (p :> string) = "" then accu
          else
            (Utf8.capitalize_fst
               (Util.relation_type_text conf rp.r_type 2
                 : Adef.safe_string
                 :> string)
            ^<^ transl conf ":" ^<^ p)
            :: accu
      | None, Some ip ->
          let p = person_of_iper conf base ip in
          if (p :> string) = "" then accu
          else
            (Utf8.capitalize_fst
               (Util.relation_type_text conf rp.r_type 2
                 : Adef.safe_string
                 :> string)
            ^<^ transl conf ":" ^<^ p)
            :: accu
      | None, None -> accu)
    rparents []
  |> (fun s -> String.concat ", " (s : Adef.safe_string list :> string list))
  |> Adef.safe

let string_of_marriage conf marriage =
  let s =
    match marriage with
    | NotMarried | NoSexesCheckNotMarried -> "with"
    | Married | NoSexesCheckMarried -> "married"
    | Engaged -> "engaged"
    | NoMention | MarriageBann | MarriageContract | MarriageLicense | Pacs
    | Residence ->
        "with"
  in
  Adef.safe (transl conf s)

let string_of_divorce conf divorce =
  match divorce with
  | Divorced cod -> transl conf "divorced" ^<^ " " ^<^ string_of_cdate conf cod
  | Separated cod ->
      transl conf "separated" ^<^ " " ^<^ string_of_cdate conf cod
  | Separated_old -> transl conf "separated" |> Adef.safe
  | NotDivorced -> "" |> Adef.safe
  | NotSeparated -> "" |> Adef.safe

let string_of_event_witness conf base witnesses =
  Array.fold_right
    (fun (ip, wk) accu ->
      let witn = person_of_iper conf base ip in
      let kind =
        Util.string_of_witness_kind conf
          (Driver.get_sex @@ Driver.poi base ip)
          wk
      in
      if (witn :> string) = "" then (kind ^^^ transl conf ":" ^<^ witn) :: accu
      else accu)
    witnesses []
  |> fun s ->
  String.concat ", " (s : Adef.safe_string list :> string list) |> Adef.safe

let string_of_epers_name conf epers_name =
  match epers_name with
  | Epers_Birth -> Adef.safe @@ Utf8.capitalize_fst (transl conf "birth")
  | Epers_Baptism -> Adef.safe @@ Utf8.capitalize_fst (transl conf "baptism")
  | Epers_Death -> Adef.safe @@ Utf8.capitalize_fst (transl conf "death")
  | Epers_Burial -> Adef.safe @@ Utf8.capitalize_fst (transl conf "burial")
  | Epers_Cremation ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "cremation")
  | Epers_Accomplishment ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "accomplishment")
  | Epers_Acquisition ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "acquisition")
  | Epers_Adhesion -> Adef.safe @@ Utf8.capitalize_fst (transl conf "adhesion")
  | Epers_BaptismLDS ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "baptismLDS")
  | Epers_BarMitzvah ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "bar mitzvah")
  | Epers_BatMitzvah ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "bat mitzvah")
  | Epers_Benediction ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "benediction")
  | Epers_ChangeName ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "change name")
  | Epers_Circumcision ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "circumcision")
  | Epers_Confirmation ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "confirmation")
  | Epers_ConfirmationLDS ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "confirmation LDS")
  | Epers_Decoration ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "decoration")
  | Epers_DemobilisationMilitaire ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "demobilisationMilitaire")
  | Epers_Diploma -> Adef.safe @@ Utf8.capitalize_fst (transl conf "diploma")
  | Epers_Distinction ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "distinction")
  | Epers_Dotation -> Adef.safe @@ Utf8.capitalize_fst (transl conf "dotation")
  | Epers_DotationLDS ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "dotationLDS")
  | Epers_Education ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "education")
  | Epers_Election -> Adef.safe @@ Utf8.capitalize_fst (transl conf "election")
  | Epers_Emigration ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "emigration")
  | Epers_Excommunication ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "excommunication")
  | Epers_FamilyLinkLDS ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "familyLinkLDS")
  | Epers_FirstCommunion ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "firstCommunion")
  | Epers_Funeral -> Adef.safe @@ Utf8.capitalize_fst (transl conf "funeral")
  | Epers_Graduate -> Adef.safe @@ Utf8.capitalize_fst (transl conf "graduate")
  | Epers_Hospitalisation ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "hospitalisation")
  | Epers_Illness -> Adef.safe @@ Utf8.capitalize_fst (transl conf "illness")
  | Epers_Immigration ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "immigration")
  | Epers_ListePassenger ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "listePassenger")
  | Epers_MilitaryDistinction ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "militaryDistinction")
  | Epers_MilitaryPromotion ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "militaryPromotion")
  | Epers_MilitaryService ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "militaryService")
  | Epers_MobilisationMilitaire ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "mobilisationMilitaire")
  | Epers_Naturalisation ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "naturalisation")
  | Epers_Occupation ->
      Adef.safe
      @@ Utf8.capitalize_fst (transl_nth conf "occupation/occupations" 0)
  | Epers_Ordination ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "ordination")
  | Epers_Property -> Adef.safe @@ Utf8.capitalize_fst (transl conf "property")
  | Epers_Recensement ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "recensement")
  | Epers_Residence ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "residence")
  | Epers_Retired -> Adef.safe @@ Utf8.capitalize_fst (transl conf "retired")
  | Epers_ScellentChildLDS ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "scellentChildLDS")
  | Epers_ScellentParentLDS ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "scellentParentLDS")
  | Epers_ScellentSpouseLDS ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "scellentSpouseLDS")
  | Epers_VenteBien ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "venteBien")
  | Epers_Will -> Adef.safe @@ Utf8.capitalize_fst (transl conf "will")
  | Epers_Name n ->
      Adef.safe
      @@ Utf8.capitalize_fst (escape_html n : Adef.safe_string :> string)

let string_of_efam_name conf efam_name =
  match efam_name with
  | Efam_Marriage ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "marriage event")
  | Efam_NoMarriage ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "no marriage event")
  | Efam_NoMention ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "no mention")
  | Efam_Engage -> Adef.safe @@ Utf8.capitalize_fst (transl conf "engage event")
  | Efam_Divorce ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "divorce event")
  | Efam_Separated ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "separate event")
  | Efam_Annulation ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "annulation")
  | Efam_MarriageBann ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "marriage bann")
  | Efam_MarriageContract ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "marriage contract")
  | Efam_MarriageLicense ->
      Adef.safe @@ Utf8.capitalize_fst (transl conf "marriage licence")
  | Efam_PACS -> Adef.safe @@ Utf8.capitalize_fst (transl conf "PACS")
  | Efam_Residence -> Adef.safe @@ Utf8.capitalize_fst (transl conf "residence")
  | Efam_Name n ->
      Adef.safe
      @@ Utf8.capitalize_fst (escape_html n : Adef.safe_string :> string)

(* ************************************************************************ *)
(*  [Fonc] highlight_diff : char array -> bool array -> string              *)

(* ************************************************************************ *)

(** [Description] : Converti un tableau de char en string, avec les parties
    modifiées encadrées par des balises <span>. [Args] :
    - arr : tableau à convertir
    - diff_arr : tableau des différences [Retour] :
    - string [Rem] : Non exporté en clair hors de ce module. *)
let highlight_diff arr diff_arr =
  let rec loop i s =
    if i >= Array.length arr then s
    else if diff_arr.(i) then (
      let j = ref i in
      let accu = ref s in
      accu := !accu ^ "<span class=\"mark\">";
      while !j < Array.length diff_arr && diff_arr.(!j) do
        accu := !accu ^ Printf.sprintf "%c" arr.(!j);
        incr j
      done;
      accu := !accu ^ "</span>";
      loop !j !accu)
    else loop (i + 1) (s ^ Printf.sprintf "%c" arr.(i))
  in
  loop 0 ""

(* ************************************************************************ *)
(*  [Fonc] array_of_string : string -> char array                           *)

(* ************************************************************************ *)

(** [Description] : Converti une string en tableau de char afin de pouvoir faire
    un diff. [Args] :
    - s : string à convertir [Retour] :
    - char array [Rem] : Non exporté en clair hors de ce module. *)
let array_of_string s =
  let s = (s :> string) in
  let len = String.length s in
  let a = Array.make len ' ' in
  let rec loop i =
    if i = len then a
    else (
      a.(i) <- s.[i];
      loop (i + 1))
  in
  loop 0

let diff_string (before : Adef.safe_string) (after : Adef.safe_string) :
    Adef.safe_string * Adef.safe_string =
  if before = after then (before, after)
  else if (before :> string) = "" then
    (before, "<span class=\"mark\">" ^<^ after ^>^ "</span>")
  else if (after :> string) = "" then
    ("<span class=\"mark\">" ^<^ before ^>^ "</span>", after)
  else
    let aa = array_of_string (after :> string) in
    let bb = array_of_string (before :> string) in
    let bef_d, aft_d = Difference.f bb aa in
    let bef_s = highlight_diff bb bef_d in
    let aft_s = highlight_diff aa aft_d in
    (Adef.safe bef_s, Adef.safe aft_s)

type 'a env =
  | Vfam of
      (Driver.iper, Driver.ifam, string) gen_family option
      * (Driver.iper, Driver.ifam, string) gen_family option
      * bool
  | Vchild of Driver.iper array option * Driver.iper array option
  | Vfevent of
      (Driver.iper, string) gen_fam_event option
      * (Driver.iper, string) gen_fam_event option
      * bool
  | Vpevent of
      (Driver.iper, string) gen_pers_event option
      * (Driver.iper, string) gen_pers_event option
  | Vint of int
  | Vstring of string
  | Vother of 'a
  | Vnone

let get_env v env = try Templ.Env.find v env with Not_found -> Vnone
let get_vother = function Vother x -> Some x | _ -> None
let set_vother x = Vother x
let str_val x = Templ.VVstring x
let safe_val (x : Adef.safe_string) = Templ.VVstring (x :> string)

let rec eval_var conf base env (bef, aft, p_auth) _loc sl =
  try eval_simple_var conf base env (bef, aft, p_auth) sl
  with Not_found -> eval_compound_var conf base env (bef, aft, p_auth) sl

and eval_simple_var conf base env (bef, aft, p_auth) :
    string list -> 'a Templ.expr_val = function
  | [ s ] -> eval_simple_str_var conf base env (bef, aft, p_auth) s
  | _ -> raise Not_found

and eval_compound_var conf base env (bef, aft, p_auth) sl : 'b Templ.expr_val =
  let loop = function
    | [ s ] -> eval_simple_str_var conf base env (bef, aft, p_auth) s
    | [ "evar"; s ] -> (
        match p_getenv conf.env s with
        | Some s -> safe_val (escape_html s)
        | None -> str_val "")
    | "before" :: sl ->
        fst (eval_gen_record conf base env (bef, aft, p_auth) sl)
    | "after" :: sl -> snd (eval_gen_record conf base env (bef, aft, p_auth) sl)
    | _ -> raise Not_found
  in
  loop sl

and eval_gen_record conf base env (bef, aft, p_auth) :
    string list -> 'a Templ.expr_val * 'b Templ.expr_val = function
  | [ "date" ] -> (safe_val bef.date, safe_val aft.date)
  | [ "wizard" ] ->
      (safe_val bef.HistoryDiff.wizard, safe_val aft.HistoryDiff.wizard)
  | [ s ] -> eval_str_gen_record conf base env (bef, aft, p_auth) s
  | _ -> raise Not_found

and eval_str_gen_record conf base env (bef, aft, p_auth) :
    string -> 'a Templ.expr_val * 'b Templ.expr_val =
  let diff_string a b =
    let a, b = diff_string a b in
    (safe_val a, safe_val b)
  in
  let aux g =
    if p_auth then
      diff_string (g bef :> Adef.safe_string) (g aft :> Adef.safe_string)
    else (str_val "", str_val "")
  in
  let aux' m_auth bef aft f =
    if p_auth && m_auth then
      match (bef, aft) with
      | Some b, Some a -> diff_string (f conf b) (f conf a)
      | None, Some a -> (str_val "", safe_val (f conf a))
      | Some b, None -> (safe_val (f conf b), str_val "")
      | None, None -> (str_val "", str_val "")
    else (str_val "", str_val "")
  in
  function
  | "first_name" -> aux (fun x -> Util.escape_html x.gen_p.first_name)
  | "surname" -> aux (fun x -> Util.escape_html x.gen_p.surname)
  | "occ" -> aux (fun x -> Adef.safe @@ string_of_int x.gen_p.occ)
  | "image" ->
      if not conf.no_image then aux (fun x -> Util.escape_html x.gen_p.image)
      else (str_val "", str_val "")
  | "public_name" -> aux (fun x -> Util.escape_html x.gen_p.public_name)
  | "qualifiers" ->
      aux (fun x -> Util.escape_html @@ String.concat ", " x.gen_p.qualifiers)
  | "aliases" ->
      aux (fun x -> Util.escape_html @@ String.concat ", " x.gen_p.aliases)
  | "first_names_aliases" ->
      aux (fun x ->
          Util.escape_html @@ String.concat ", " x.gen_p.first_names_aliases)
  | "surnames_aliases" ->
      aux (fun x ->
          Util.escape_html @@ String.concat ", " x.gen_p.surnames_aliases)
  | "titles" -> aux (fun x -> string_of_title conf x.gen_p.titles)
  | "relations" ->
      aux (fun x ->
          let r =
            string_of_related conf base x.gen_p.key_index x.gen_p.related
          in
          let rp = string_of_rparents conf base x.gen_p.rparents in
          if (r :> string) = "" then rp else r ^^^ ". " ^<^ rp)
  | "occupation" -> aux (fun x -> Util.safe_html x.gen_p.occupation)
  | "sex" ->
      aux (fun x ->
          Util.index_of_sex x.gen_p.sex
          |> transl_nth conf "male/female/neuter"
          |> Adef.safe)
  | "access" ->
      aux (fun x ->
          match x.gen_p.access with
          | IfTitles ->
              transl_nth conf "iftitles/public/semipublic/private" 0
              |> Adef.safe
          | Public ->
              transl_nth conf "iftitles/public/semipublic/private" 1
              |> Adef.safe
          | SemiPublic ->
              transl_nth conf "iftitles/public/semipublic/private" 2
              |> Adef.safe
          | Private ->
              transl_nth conf "iftitles/public/semipublic/private" 3
              |> Adef.safe)
  | "birth" -> aux (fun x -> string_of_cdate conf x.gen_p.birth)
  | "birth_place" -> aux (fun x -> Util.escape_html x.gen_p.birth_place)
  | "birth_note" -> aux (fun x -> Util.escape_html x.gen_p.birth_note)
  | "birth_src" -> aux (fun x -> Util.escape_html x.gen_p.birth_src)
  | "baptism" -> aux (fun x -> string_of_cdate conf x.gen_p.baptism)
  | "baptism_place" -> aux (fun x -> Util.escape_html x.gen_p.baptism_place)
  | "baptism_note" -> aux (fun x -> Util.escape_html x.gen_p.baptism_note)
  | "baptism_src" -> aux (fun x -> Util.escape_html x.gen_p.baptism_src)
  | "death" -> aux (fun x -> string_of_death conf x.gen_p.death)
  | "death_place" -> aux (fun x -> Util.escape_html x.gen_p.death_place)
  | "death_note" -> aux (fun x -> Util.escape_html x.gen_p.death_note)
  | "death_src" -> aux (fun x -> Util.escape_html x.gen_p.death_src)
  | "burial" -> aux (fun x -> string_of_burial conf x.gen_p.burial)
  | "burial_place" -> aux (fun x -> Util.escape_html x.gen_p.burial_place)
  | "burial_note" -> aux (fun x -> Util.escape_html x.gen_p.burial_note)
  | "burial_src" -> aux (fun x -> Util.escape_html x.gen_p.burial_src)
  | "pevent_name" -> (
      match get_env "pevent" env with
      | Vpevent (bef, aft) ->
          aux' true bef aft (fun conf x ->
              string_of_epers_name conf x.epers_name)
      | _ -> raise Not_found)
  | "pevent_date" -> (
      match get_env "pevent" env with
      | Vpevent (bef, aft) ->
          aux' true bef aft (fun conf x -> string_of_cdate conf x.epers_date)
      | _ -> raise Not_found)
  | "pevent_place" -> (
      match get_env "pevent" env with
      | Vpevent (bef, aft) ->
          aux' true bef aft (fun _ x -> escape_html x.epers_place)
      | _ -> raise Not_found)
  | "pevent_note" -> (
      match get_env "pevent" env with
      | Vpevent (bef, aft) ->
          aux' (not conf.no_note) bef aft (fun _ x -> escape_html x.epers_note)
      | _ -> raise Not_found)
  | "pevent_src" -> (
      match get_env "pevent" env with
      | Vpevent (bef, aft) ->
          aux' true bef aft (fun _ x -> escape_html x.epers_src)
      | _ -> raise Not_found)
  | "pevent_witness" -> (
      match get_env "pevent" env with
      | Vpevent (bef, aft) ->
          aux' true bef aft (fun conf x ->
              string_of_event_witness conf base x.epers_witnesses)
      | _ -> raise Not_found)
  | "notes" ->
      if not conf.no_note then aux (fun x -> Util.escape_html x.gen_p.notes)
      else (str_val "", str_val "")
  | "psources" -> aux (fun x -> Util.escape_html x.gen_p.psources)
  | "spouse" -> (
      match get_env "fam" env with
      | Vfam (_f_bef, _f_aft, m_auth) ->
          if m_auth then
            (eval_string_env "spouse_bef" env, eval_string_env "spouse_aft" env)
          else (str_val "", str_val "")
      | _ -> raise Not_found)
  | "marriage" -> (
      match get_env "fam" env with
      | Vfam (bef, aft, m_auth) ->
          aux' m_auth bef aft (fun conf x -> string_of_cdate conf x.marriage)
      | _ -> raise Not_found)
  | "marriage_place" -> (
      match get_env "fam" env with
      | Vfam (bef, aft, m_auth) ->
          aux' m_auth bef aft (fun _ x -> escape_html x.marriage_place)
      | _ -> raise Not_found)
  | "marriage_src" -> (
      match get_env "fam" env with
      | Vfam (bef, aft, m_auth) ->
          aux' m_auth bef aft (fun _ x -> escape_html x.marriage_src)
      | _ -> raise Not_found)
  | "witnesses" -> (
      match get_env "fam" env with
      | Vfam (bef, aft, m_auth) ->
          aux' m_auth bef aft (fun conf x ->
              person_of_iper_array conf base x.witnesses)
      | _ -> raise Not_found)
  | "marriage_type" -> (
      match get_env "fam" env with
      | Vfam (bef, aft, m_auth) ->
          aux' m_auth bef aft (fun conf x -> string_of_marriage conf x.relation)
      | _ -> raise Not_found)
  | "divorce" -> (
      match get_env "fam" env with
      | Vfam (bef, aft, m_auth) ->
          aux' m_auth bef aft (fun conf x -> string_of_divorce conf x.divorce)
      | _ -> raise Not_found)
  | "fevent_name" -> (
      match get_env "fevent" env with
      | Vfevent (bef, aft, m_auth) ->
          aux' m_auth bef aft (fun conf x ->
              string_of_efam_name conf x.efam_name)
      | _ -> raise Not_found)
  | "fevent_date" -> (
      match get_env "fevent" env with
      | Vfevent (bef, aft, m_auth) ->
          aux' m_auth bef aft (fun conf x -> string_of_cdate conf x.efam_date)
      | _ -> raise Not_found)
  | "fevent_place" -> (
      match get_env "fevent" env with
      | Vfevent (bef, aft, m_auth) ->
          aux' m_auth bef aft (fun _ x -> escape_html x.efam_place)
      | _ -> raise Not_found)
  | "fevent_note" -> (
      match get_env "fevent" env with
      | Vfevent (bef, aft, m_auth) ->
          aux' (m_auth && not conf.no_note) bef aft (fun _ x ->
              escape_html x.efam_note)
      | _ -> raise Not_found)
  | "fevent_src" -> (
      match get_env "fevent" env with
      | Vfevent (bef, aft, m_auth) ->
          aux' m_auth bef aft (fun _ x -> escape_html x.efam_src)
      | _ -> raise Not_found)
  | "fevent_witness" -> (
      match get_env "fevent" env with
      | Vfevent (bef, aft, m_auth) ->
          aux' m_auth bef aft (fun conf x ->
              string_of_event_witness conf base x.efam_witnesses)
      | _ -> raise Not_found)
  | "comment" -> (
      match get_env "fam" env with
      | Vfam (bef, aft, m_auth) ->
          aux' (m_auth && not conf.no_note) bef aft (fun _ x ->
              escape_html x.comment)
      | _ -> raise Not_found)
  | "origin_file" -> (
      match get_env "fam" env with
      | Vfam (bef, aft, m_auth) ->
          aux' m_auth bef aft (fun _ x -> escape_html x.origin_file)
      | _ -> raise Not_found)
  | "fsources" -> (
      match get_env "fam" env with
      | Vfam (bef, aft, m_auth) ->
          aux' m_auth bef aft (fun _ x -> escape_html x.fsources)
      | _ -> raise Not_found)
  | "children" -> (
      match get_env "fam" env with
      | Vfam (_, _, m_auth) ->
          if m_auth then
            match get_env "child" env with
            | Vchild (bef, aft) ->
                aux' true bef aft (fun conf -> person_of_iper_array conf base)
            | _ -> raise Not_found
          else (str_val "", str_val "")
      | _ -> raise Not_found)
  | _ -> raise Not_found

and eval_simple_str_var conf base env (bef, aft, p_auth) :
    string -> 'a Templ.expr_val = function
  | "acces" ->
      person_of_gen_p_key base aft.gen_p
      |> acces conf base
      |> (safe_val :> Adef.escaped_string -> 'a Templ.expr_val)
  | "date" -> eval_string_env "date" env
  | "history_len" -> eval_int_env "history_len" env
  | "line" -> eval_int_env "line" env
  | "nb_families" ->
      max (List.length bef.gen_f) (List.length aft.gen_f)
      |> string_of_int |> str_val
  | "person" ->
      if p_auth then
        person_of_gen_p_key base aft.gen_p
        |> Util.gen_person_text conf base
        |> safe_val
      else eval_string_env "history_file" env
  | "wizard" -> eval_string_env "wizard" env
  | _ -> raise Not_found

and eval_string_env s env =
  match get_env s env with
  | Vstring s -> str_val s (* FIXME? *)
  | _ -> raise Not_found

and eval_int_env s env =
  match get_env s env with
  | Vint i -> str_val (string_of_int i)
  | _ -> raise Not_found

let print_foreach conf base print_ast _eval_expr =
  let rec print_foreach env xx _loc s sl _el al =
    match s :: sl with
    | [ "family" ] -> print_foreach_family env xx al
    | [ "fevent" ] -> print_foreach_fevent env xx al
    | [ "pevent" ] -> print_foreach_pevent env xx al
    | [ "history_line" ] -> print_foreach_history_line env xx al
    | _ -> raise Not_found
  and print_foreach_family env xx al =
    let bef, aft, p_auth = xx in
    let rec loop bef_f bef_c aft_f aft_c =
      match (bef_f, aft_f) with
      | [], [] -> ()
      | [], gen_f :: l ->
          let fam = Driver.foi base gen_f.fam_index in
          let isp = Gutil.spouse aft.gen_p.key_index fam in
          let sp = person_of_iper conf base isp in
          let m_auth =
            authorized_age conf base (Driver.poi base isp) && p_auth
          in
          let vfam = Vfam (None, Some gen_f, m_auth) in
          let vchild, c =
            match (bef_c, aft_c) with
            | [], gen_c :: l -> (Vchild (None, Some gen_c), l)
            | _ -> (* pas normal*) (Vchild (None, None), [])
          in
          let env =
            Templ.Env.(
              env |> add "fam" vfam
              |> add "spouse_bef" (Vstring "")
              |> add "spouse_aft" (Vstring (sp :> string))
              |> add "child" vchild)
          in
          List.iter (print_ast env xx) al;
          loop [] bef_c l c
      | gen_f :: l, [] ->
          let fam = Driver.foi base gen_f.fam_index in
          let isp = Gutil.spouse aft.gen_p.key_index fam in
          let sp = person_of_iper conf base isp in
          let m_auth =
            authorized_age conf base (Driver.poi base isp) && p_auth
          in
          let vfam = Vfam (Some gen_f, None, m_auth) in
          let vchild, c =
            match (bef_c, aft_c) with
            | gen_c :: l, [] -> (Vchild (Some gen_c, None), l)
            | _ -> (* pas normal*) (Vchild (None, None), [])
          in
          let env =
            Templ.Env.(
              env |> add "fam" vfam
              |> add "spouse_bef" (Vstring (sp :> string))
              |> add "spouse_aft" (Vstring "")
              |> add "child" vchild)
          in
          List.iter (print_ast env xx) al;
          loop l c [] aft_c
      | gen_f1 :: l1, gen_f2 :: l2 ->
          let fam = Driver.foi base gen_f2.fam_index in
          let isp1 = Gutil.spouse bef.gen_p.key_index fam in
          let isp2 = Gutil.spouse aft.gen_p.key_index fam in
          let sp1 = person_of_iper conf base isp1 in
          let sp2 = person_of_iper conf base isp2 in
          let m_auth =
            authorized_age conf base (Driver.poi base isp2) && p_auth
          in
          let vfam = Vfam (Some gen_f1, Some gen_f2, m_auth) in
          let vchild, c1, c2 =
            match (bef_c, aft_c) with
            | gen_c1 :: l1, gen_c2 :: l2 ->
                (Vchild (Some gen_c1, Some gen_c2), l1, l2)
            | _ -> (* pas normal*) (Vchild (None, None), [], [])
          in
          let env =
            Templ.Env.(
              env |> add "fam" vfam
              |> add "spouse_bef" (Vstring (sp1 :> string))
              |> add "spouse_aft" (Vstring (sp2 :> string))
              |> add "child" vchild)
          in
          List.iter (print_ast env xx) al;
          loop l1 c1 l2 c2
    in
    loop bef.gen_f bef.gen_c aft.gen_f aft.gen_c
  and print_foreach_fevent env xx al =
    let rec loop m_auth bef_fevents aft_fevents =
      match (bef_fevents, aft_fevents) with
      | [], [] -> ()
      | [], aft_evt :: l ->
          let env =
            Templ.Env.add "fevent" (Vfevent (None, Some aft_evt, m_auth)) env
          in
          List.iter (print_ast env xx) al;
          loop m_auth [] l
      | bef_evt :: l, [] ->
          let env =
            Templ.Env.add "fevent" (Vfevent (Some bef_evt, None, m_auth)) env
          in
          List.iter (print_ast env xx) al;
          loop m_auth l []
      | bef_evt :: l1, aft_evt :: l2 ->
          let env =
            Templ.Env.add "fevent"
              (Vfevent (Some bef_evt, Some aft_evt, m_auth))
              env
          in
          List.iter (print_ast env xx) al;
          loop m_auth l1 l2
    in
    match get_env "fam" env with
    | Vfam (bef, aft, m_auth) -> (
        match (bef, aft) with
        | Some b, Some a -> loop m_auth b.fevents a.fevents
        | None, Some a -> loop m_auth [] a.fevents
        | Some b, None -> loop m_auth b.fevents []
        | None, None -> ())
    | _ -> ()
  and print_foreach_pevent env xx al =
    let bef, aft, _p_auth = xx in
    let rec loop bef_pevents aft_pevents =
      match (bef_pevents, aft_pevents) with
      | [], [] -> ()
      | [], aft_evt :: l ->
          let env = Templ.Env.add "pevent" (Vpevent (None, Some aft_evt)) env in
          List.iter (print_ast env xx) al;
          loop [] l
      | bef_evt :: l, [] ->
          let env = Templ.Env.add "pevent" (Vpevent (Some bef_evt, None)) env in
          List.iter (print_ast env xx) al;
          loop l []
      | bef_evt :: l1, aft_evt :: l2 ->
          let env =
            Templ.Env.add "pevent" (Vpevent (Some bef_evt, Some aft_evt)) env
          in
          List.iter (print_ast env xx) al;
          loop l1 l2
    in
    loop bef.gen_p.pevents aft.gen_p.pevents
  and print_foreach_history_line env xx al =
    match get_env "history_file" env with
    | Vstring fname ->
        let history = load_person_history conf fname in
        let rec loop i list =
          match list with
          | [] -> ()
          | gr :: l ->
              let env =
                Templ.Env.(
                  empty |> add "line" (Vint i)
                  |> add "date" (Vstring (gr.date : Adef.safe_string :> string))
                  |> add "wizard"
                       (Vstring
                          (gr.HistoryDiff.wizard : Adef.safe_string :> string)))
              in
              List.iter (print_ast env xx) al;
              loop (i + 1) l
        in
        loop 0 history
    | _ -> ()
  in
  print_foreach

let eval_predefined_apply conf _env f vl =
  let vl =
    List.map (function Templ.VVstring s -> s | _ -> raise Not_found) vl
  in
  match (f, vl) with
  | "transl_date", [ date_txt ] -> (
      (* date_tpl = "0000-00-00 00:00:00" *)
      try
        let year = int_of_string (String.sub date_txt 0 4) in
        let month = int_of_string (String.sub date_txt 5 2) in
        let day = int_of_string (String.sub date_txt 8 2) in
        let date =
          Dgreg ({ day; month; year; prec = Sure; delta = 0 }, Dgregorian)
        in
        let time = String.sub date_txt 11 8 in
        DateDisplay.string_of_date conf date ^>^ ", " ^ time
      with Failure _ -> escape_html date_txt)
  | _ -> raise Not_found

let print conf base =
  match p_getenv conf.env "t" with
  | Some ("SUM" | "DIFF") -> (
      match p_getenv conf.env "f" with
      | Some file when file <> "" ->
          let history = load_person_history conf file in
          let len = List.length history in
          let before, after =
            match (p_getint conf.env "old", p_getint conf.env "new") with
            | Some o, Some n ->
                let o =
                  if o < 0 then 0 else if o > len - 1 then len - 1 else o
                in
                let n =
                  if n < 0 then 0 else if n > len - 1 then len - 1 else n
                in
                (o, n)
            | _ -> (0, 0)
          in
          let before = List.nth history before in
          let after = List.nth history after in
          let p = person_of_gen_p_key base after.gen_p in
          let p_auth = authorized_age conf base p in
          let env =
            Templ.Env.(
              empty
              |> add "history_file" (Vstring file)
              |> add "history_len" (Vint len))
          in
          let eval_predefined_apply _env f vl =
            (eval_predefined_apply conf _env f vl :> string)
          in
          let ifun =
            Templ.
              {
                eval_var = eval_var conf base;
                eval_transl = (fun _ -> Templ.eval_transl conf);
                eval_predefined_apply;
                get_vother;
                set_vother;
                print_foreach = print_foreach conf base;
              }
          in
          Templ.output conf ifun env (before, after, p_auth) "updhist_diff"
      | _ -> Hutil.incorrect_request conf ~comment:"bad f parameter")
  | _ -> Hutil.incorrect_request conf ~comment:"bad t parameter"

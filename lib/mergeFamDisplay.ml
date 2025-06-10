(* $Id: mergeFam.ml,v 5.13 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
module Driver = Geneweb_db.Driver

let compatible_fevents fevt1 fevt2 = fevt1 = [] && fevt2 = []

let need_differences_selection conf base fam1 fam2 =
  let need_selection proj =
    let x1 = proj fam1 in
    let x2 = proj fam2 in
    x1 <> "" && x2 <> "" && x1 <> x2
  in
  need_selection (fun fam ->
      match Driver.get_relation fam with
      | Married -> "married"
      | NotMarried -> "not married"
      | Engaged -> "engaged"
      | NoSexesCheckNotMarried -> "no sexes check"
      | NoSexesCheckMarried -> "no sexes check married"
      | NoMention -> "no mention"
      | MarriageBann -> "marriage banns"
      | MarriageContract -> "marriage contract"
      | MarriageLicense -> "marriage license"
      | Pacs -> "pacs"
      | Residence -> "residence")
  || need_selection (fun fam ->
         match Date.od_of_cdate (Driver.get_marriage fam) with
         | None -> ""
         | Some d -> (DateDisplay.string_of_ondate conf d :> string))
  || need_selection (fun fam -> Driver.sou base (Driver.get_marriage_place fam))
  || need_selection (fun fam ->
         match Driver.get_divorce fam with
         | Divorced cod -> (
             match Date.od_of_cdate cod with
             | Some d -> (DateDisplay.string_of_ondate conf d :> string)
             | None -> "divorced")
         | _ -> "not divorced")
  || need_selection (fun fam ->
         match Driver.get_separation fam with
         | Separated cod -> (
             match Date.od_of_cdate cod with
             | Some d -> (DateDisplay.string_of_ondate conf d :> string)
             | None -> "separated")
         | Separated_old -> "separated"
         | _ -> "not separated")

let print_differences conf base branches (ifam1, fam1) (ifam2, fam2) =
  let string_field (title : Adef.safe_string) (name : Adef.encoded_string) proj
      =
    let x1 : Adef.safe_string = proj fam1 in
    let x2 : Adef.safe_string = proj fam2 in
    if (x1 :> string) <> "" && (x2 :> string) <> "" && x1 <> x2 then (
      Output.print_sstring conf "<h4>";
      Output.print_string conf (Adef.safe_fn Utf8.capitalize_fst title);
      Output.print_sstring conf
        "</h4><ul><li><input type=\"radio\" class=\"form-control\" name=\"";
      Output.print_string conf name;
      Output.print_sstring conf "\" value=\"1\" checked>";
      Output.print_string conf x1;
      Output.print_sstring conf
        "</li><li><input type=\"radio\" class=\"form-control\" name=\"";
      Output.print_string conf name;
      Output.print_sstring conf "\" value=\"2\">";
      Output.print_string conf x2;
      Output.print_sstring conf "</li></ul>")
  in
  Output.print_sstring conf "<form method=\"post\" action=\"";
  Output.print_sstring conf conf.command;
  Output.print_sstring conf "\">";
  Util.hidden_env conf;
  Util.hidden_input_s conf "m" "MRG_FAM_OK";
  Util.hidden_input_s conf "i" (Driver.Ifam.to_string ifam1);
  Util.hidden_input_s conf "i2" (Driver.Ifam.to_string ifam2);
  (match p_getenv conf.env "ip" with
  | Some ip -> Util.hidden_input conf "ip" (Mutil.encode ip)
  | None -> ());
  (let rec loop = function
     | [ (ip1, ip2) ] ->
         Util.hidden_input_s conf "ini1" (Driver.Iper.to_string ip1);
         Util.hidden_input_s conf "ini2" (Driver.Iper.to_string ip2)
     | _ :: branches -> loop branches
     | _ -> ()
   in
   loop branches);
  Output.print_sstring conf "<p>";
  string_field
    (transl_nth conf "relation/relations" 0 |> Adef.safe)
    (Adef.encoded "relation")
    (fun fam ->
      Adef.safe @@ transl conf
      @@
      match Driver.get_relation fam with
      | Married -> "married"
      | NotMarried -> "not married"
      | Engaged -> "engaged"
      | NoSexesCheckNotMarried -> "no sexes check"
      | NoSexesCheckMarried -> "married"
      | NoMention -> "no mention"
      | MarriageBann -> "marriage bann"
      | MarriageContract -> "marriage contract"
      | MarriageLicense -> "marriage licence"
      | Pacs -> "PACS"
      | Residence -> "residence");
  string_field
    (Util.translate_eval (transl_nth conf "marriage/marriages" 0) |> Adef.safe)
    (Adef.encoded "marriage")
    (fun fam ->
      match Date.od_of_cdate (Driver.get_marriage fam) with
      | None -> Adef.safe ""
      | Some d -> DateDisplay.string_of_ondate conf d);
  string_field
    (Util.translate_eval (transl_nth conf "marriage/marriages" 0)
     ^ " / "
     ^ transl_nth conf "place/places" 0
    |> Adef.safe)
    (Adef.encoded "marriage_place")
    (fun fam ->
      (Driver.get_marriage_place fam |> Driver.sou base |> escape_html
        :> Adef.safe_string));
  string_field
    (transl conf "divorce" |> Adef.safe)
    (Adef.encoded "divorce")
    (fun fam ->
      match Driver.get_divorce fam with
      | Divorced cod -> (
          match Date.od_of_cdate cod with
          | Some d ->
              transl conf "divorced" ^<^ " "
              ^<^ DateDisplay.string_of_ondate conf d
          | None -> transl conf "divorced" |> Adef.safe)
      | _ -> transl conf "not divorced" |> Adef.safe);
  string_field
    (transl conf "separation" |> Adef.safe)
    (Adef.encoded "separation")
    (fun fam ->
      match Driver.get_separation fam with
      | Separated cod -> (
          match Date.od_of_cdate cod with
          | Some d ->
              transl conf "separated" ^<^ " "
              ^<^ DateDisplay.string_of_ondate conf d
          | None -> transl conf "separated" |> Adef.safe)
      | Separated_old -> transl conf "separated" |> Adef.safe
      | _ -> transl conf "not divorced" |> Adef.safe);
  Output.print_sstring conf
    {|</p><p><button type="submit" class="btn btn-primary btn-lg">|};
  Output.print_sstring conf
    (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0));
  Output.print_sstring conf "</button></form>"

let merge_fam1 conf base fam1 fam2 =
  let title _ =
    let s = transl_nth conf "family/families" 1 in
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl_decline conf "merge" s))
  in
  Hutil.header conf title;
  print_differences conf base [] fam1 fam2;
  Hutil.trailer conf

let merge_fam conf base (ifam1, fam1) (ifam2, fam2) =
  let cpl1 = Driver.foi base ifam1 in
  let cpl2 = Driver.foi base ifam2 in
  (* Vérifie que les deux couples sont identiques. Il est possible dans
     certains cas (couple de même sexe) que les personnes soient inversées
     dans l'union. *)
  if
    Driver.get_father cpl1 = Driver.get_father cpl2
    && Driver.get_mother cpl1 = Driver.get_mother cpl2
    || Driver.get_father cpl1 = Driver.get_mother cpl2
       && Driver.get_mother cpl1 = Driver.get_father cpl2
  then
    if
      need_differences_selection conf base fam1 fam2
      && compatible_fevents (Driver.get_fevents fam1) (Driver.get_fevents fam2)
    then merge_fam1 conf base (ifam1, fam1) (ifam2, fam2)
    else MergeFamOk.print_merge conf base
  else Hutil.incorrect_request conf

let print conf base =
  match (p_getenv conf.env "i", p_getenv conf.env "i2") with
  | Some f1, Some f2 ->
      let ifam1 = Driver.Ifam.of_string f1 in
      let ifam2 = Driver.Ifam.of_string f2 in
      let fam1 = Driver.foi base ifam1 in
      let fam2 = Driver.foi base ifam2 in
      merge_fam conf base (ifam1, fam1) (ifam2, fam2)
  | _ -> Hutil.incorrect_request conf

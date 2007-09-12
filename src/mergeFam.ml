(* camlp5r ./pa_html.cmo *)
(* $Id: mergeFam.ml,v 5.13 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;
open Hutil;
open Util;

value need_differences_selection conf base fam1 fam2 =
  let need_selection proj =
    let x1 = proj fam1 in
    let x2 = proj fam2 in x1 <> "" && x2 <> "" && x1 <> x2
  in
  need_selection
    (fun fam ->
       match get_relation fam with
       [ Married -> "married"
       | NotMarried -> "not married"
       | Engaged -> "engaged"
       | NoSexesCheckNotMarried -> "no sexes check"
       | NoSexesCheckMarried -> "no sexes check married"
       | NoMention -> "no mention" ]) ||
  need_selection
    (fun fam ->
       match Adef.od_of_codate (get_marriage fam) with
       [ None -> ""
       | Some d -> Date.string_of_ondate conf d ]) ||
  need_selection (fun fam -> sou base (get_marriage_place fam)) ||
  need_selection
    (fun fam ->
       match get_divorce fam with
       [ NotDivorced -> "not divorced"
       | Separated -> "separated"
       | Divorced cod ->
           match Adef.od_of_codate cod with
           [ Some d -> Date.string_of_ondate conf d
           | None -> "divorced" ] ])
;

value print_differences conf base branches (ifam1, fam1) (ifam2, fam2) =
  let string_field title name proj =
    let x1 = proj fam1 in
    let x2 = proj fam2 in
    if x1 <> "" && x2 <> "" && x1 <> x2 then do {
      Wserver.wprint "<h4>%s</h4>\n" (capitale title);
      tag "ul" begin
        html_li conf;
        Wserver.wprint
          "<input type=\"radio\" name=\"%s\" value=\"1\" checked>\n"
          name;
        Wserver.wprint "%s\n" x1;
        html_li conf;
        Wserver.wprint "<input type=\"radio\" name=\"%s\" value=\"2\">\n" name;
        Wserver.wprint "%s\n" x2;
      end;
    }
    else ()
  in
  tag "form" "method=\"post\" action=\"%s\"" conf.command begin
    Util.hidden_env conf;
    Wserver.wprint "<input type=\"hidden\" name=\"m\" value=\"MRG_FAM_OK\">\n";
    Wserver.wprint "<input type=\"hidden\" name=\"i\" value=\"%d\">\n"
      (Adef.int_of_ifam ifam1);
    Wserver.wprint "<input type=\"hidden\" name=\"i2\" value=\"%d\">\n"
      (Adef.int_of_ifam ifam2);
    match p_getenv conf.env "ip" with
    [ Some ip ->
        Wserver.wprint "<input type=\"hidden\" name=\"ip\" value=\"%s\">\n" ip
    | None -> () ];
    let rec loop =
      fun
      [ [(ip1, ip2)] ->
          do {
            Wserver.wprint
              "<input type=\"hidden\" name=\"ini1\" value=\"%d\">\n"
              (Adef.int_of_iper ip1);
            Wserver.wprint
              "<input type=\"hidden\" name=\"ini2\" value=\"%d\">\n"
              (Adef.int_of_iper ip2);
          }
      | [_ :: branches] -> loop branches
      | _ -> () ]
    in
    loop branches;
    html_p conf;
    string_field (transl_nth conf "relation/relations" 0) "relation"
      (fun fam ->
         match get_relation fam with
         [ Married -> transl conf "married"
         | NotMarried -> transl conf "not married"
         | Engaged -> transl conf "engaged"
         | NoSexesCheckNotMarried -> transl conf "no sexes check"
         | NoSexesCheckMarried -> transl conf "married"
         | NoMention -> transl conf "no mention" ]);
    string_field
      (Util.translate_eval (transl_nth conf "marriage/marriages" 0))
      "marriage"
      (fun fam ->
         match Adef.od_of_codate (get_marriage fam) with
         [ None -> ""
         | Some d -> Date.string_of_ondate conf d ]);
    string_field
      (Util.translate_eval (transl_nth conf "marriage/marriages" 0) ^ " / " ^
         transl conf "place")
      "marriage_place" (fun fam -> sou base (get_marriage_place fam));
    string_field (transl conf "divorce") "divorce"
      (fun fam ->
         match get_divorce fam with
         [ NotDivorced -> transl conf "not divorced"
         | Separated -> transl conf "separated"
         | Divorced cod ->
             let ds =
               match Adef.od_of_codate cod with
               [ Some d -> " " ^ Date.string_of_ondate conf d
               | None -> "" ]
             in
             transl conf "divorced" ^ ds ]);
    html_p conf;
    Wserver.wprint "<input type=\"submit\" value=\"Ok\">\n";
  end
;

value merge_fam1 conf base fam1 fam2 =
  let title h =
    let s = transl_nth conf "family/families" 1 in
    Wserver.wprint "%s" (capitale (transl_decline conf "merge" s))
  in
  do {
    header conf title;
    print_differences conf base [] fam1 fam2;
    trailer conf;
  }
;

value merge_fam conf base (ifam1, fam1) (ifam2, fam2) =
  let cpl1 = foi base ifam1 in
  let cpl2 = foi base ifam2 in
  if get_father cpl1 = get_father cpl2 && get_mother cpl1 = get_mother cpl2
  then
    if need_differences_selection conf base fam1 fam2 then
      merge_fam1 conf base (ifam1, fam1) (ifam2, fam2)
    else
      MergeFamOk.print_merge conf base
  else
    incorrect_request conf
;

value print conf base =
  match (p_getint conf.env "i", p_getint conf.env "i2") with
  [ (Some f1, Some f2) ->
      let ifam1 = Adef.ifam_of_int f1 in
      let ifam2 = Adef.ifam_of_int f2 in
      let fam1 = foi base ifam1 in
      let fam2 = foi base ifam2 in
      merge_fam conf base (ifam1, fam1) (ifam2, fam2)
  | _ ->
      incorrect_request conf ]
;

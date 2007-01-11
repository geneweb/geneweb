(* camlp4r ./pa_html.cmo *)
(* $Id: mergeChil.ml,v 5.1 2007-01-11 12:57:05 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config;
open Gwdb;
open Util;

value print_link conf base p = do {
  stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p) begin
    Wserver.wprint "%s.%d %s" (sou base (get_first_name p))
      (get_occ p) (sou base (get_surname p));
  end;
  Wserver.wprint "%s" (Date.short_dates_text conf base p);
  match main_title conf base p with
  [ Some t -> Wserver.wprint "%s" (one_title_text conf base p t)
  | None -> () ];
};

value print_no_candidate conf base (ip, p) = do {
  let title _ =
    Wserver.wprint "%s\n"
      (capitale
         (transl_decline conf "merge" (transl_nth conf "child/children" 1)))
  in
  Util.header conf title;
  Util.print_link_to_welcome conf True;
  Wserver.wprint "%s\n" (capitale (transl conf "not found"));
  tag "ul" begin
    tag "li" begin print_link conf base p; end;
  end;
  Util.trailer conf;
};

value print_candidates conf base (ip, p) iexcl (ip1, p1) (ip2, p2) = do {
  let title _ =
    Wserver.wprint "%s\n"
      (capitale
         (transl_decline conf "merge" (transl_nth conf "child/children" 1)))
  in
  Util.header conf title;
  Util.print_link_to_welcome conf True;
  tag "ul" begin
    tag "li" begin print_link conf base p1; end;
    tag "li" begin print_link conf base p2; end;
  end;
  tag "p" begin
    Wserver.wprint "%s ?\n" (capitale (transl conf "merge"));
    tag "form" begin
      Util.hidden_env conf;
      xtag "input" "type=\"hidden\" name=\"m\" value=\"MRG_CHN_Y_N\"";
      xtag "input" "type=\"hidden\" name=\"ip\" value=\"%d\""
        (Adef.int_of_iper ip);
      let s =
        List.fold_left
          (fun s (i1, i2) ->
             let t =
               string_of_int (Adef.int_of_iper i1) ^ "," ^
               string_of_int (Adef.int_of_iper i2)
             in
             if s = "" then t else s ^ "," ^ t)
          "" [(ip1, ip2) :: iexcl]
      in
      if s = "" then ()
      else xtag "input" "type=\"hidden\" name=\"iexcl\" value=\"%s\"" s;
      xtag "input" "type=\"hidden\" name=\"i\" value=\"%d\""
        (Adef.int_of_iper ip1);
      xtag "input" "type=\"hidden\" name=\"select\" value=\"%d\""
        (Adef.int_of_iper ip2);
      xtag "input" "type=\"submit\" name=\"answer_y\" value=\"%s\""
        (transl_nth conf "Y/N" 0);
      xtag "input" "type=\"submit\" name=\"answer_n\" value=\"%s\""
        (transl_nth conf "Y/N" 1);
    end;
  end;
  Util.trailer conf;
};

value main_page conf base =
  let ipp =
    match p_getint conf.env "ip" with
    [ Some i -> Some (Adef.iper_of_int i, poi base (Adef.iper_of_int i))
    | None -> None ]
  in
  let iexcl =
    match p_getenv conf.env "iexcl" with
    [ Some s ->
        loop [] 0 where rec loop ipl i =
          if i >= String.length s then ipl
          else
            let j =
              try String.index_from s i ',' with
              [ Not_found -> String.length s ]
            in
            if j = String.length s then ipl
            else
              let k =
                try String.index_from s (j + 1) ',' with
                [ Not_found -> String.length s ]
              in
              let s1 = String.sub s i (j - i) in
              let s2 = String.sub s (j + 1) (k - j - 1) in
              let ipl =
                match
                  try Some (int_of_string s1, int_of_string s2) with
                  [ Failure _ -> None ]
                with
                [ Some (i1, i2) ->
                    [(Adef.iper_of_int i1, Adef.iper_of_int i2) :: ipl]
                | None -> ipl ]
              in
              loop ipl (k + 1)
    | None -> [] ]
  in
  match ipp with
  [ Some (ip, p) ->
      let u = uoi base ip in
      let ipl =
        loop (Array.to_list (get_family u)) where rec loop =
          fun
          [ [ifam :: ifaml] ->
              let ipl = Array.to_list (get_children (doi base ifam)) in
              ipl @ loop ifaml
          | [] -> [] ]
      in
      let cand =
        loop_chil ipl where rec loop_chil =
          fun
          [ [ip1 :: ipl1] ->
              let p1 = poi base ip1 in
              let fn1 = get_first_name p1 in
              loop_same ipl1 where rec loop_same =
                fun
                [ [ip2 :: ipl2] ->
                    let p2 = poi base ip2 in
                    if get_first_name p2 = fn1 then
                      if List.mem (ip1, ip2) iexcl then loop_same ipl2
                      else Some ((ip1, p1), (ip2, p2))
                    else loop_same ipl2
                | [] -> loop_chil ipl1 ]
          | [] -> None ]
      in
      match cand with
      [ Some (ipp1, ipp2) ->
          print_candidates conf base (ip, p) iexcl ipp1 ipp2
      | None -> 
          print_no_candidate conf base (ip, p) ]
 | None ->
      incorrect_request conf ]
;

value answered_y_n conf base =
  let yes = p_getenv conf.env "answer_y" <> None in
  if yes then MergeInd.print conf base
  else main_page conf base
;

(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util
open Cousins

let default_max_cnt = Cousins.default_max_cnt

let brother_label conf x =
  match x with
    1 -> transl conf "siblings"
  | 2 -> transl conf "cousins"
  | 3 -> transl conf "2nd cousins"
  | 4 -> transl conf "3rd cousins"
  | n ->
    Printf.sprintf (ftransl conf "%s cousins")
      (transl_nth conf "nth (cousin)" (n - 1))

let cnt = ref 0
let cnt_sp = ref 0

let include_templ conf name =
  Util.include_template conf [] name
    (fun () -> Output.printf conf "Failed to open: %s.txt" name)

(* HTML main *)

let print_cousins conf base p lev1 lev2 =
  let title h =
    let txt_fun a =
      let txt = gen_person_text raw_access conf base p in
      transl_a_of_gr_eq_gen_lev conf a
        (if h then gen_person_text_no_html raw_access conf base p else txt)
        txt
    in
    if lev1 = lev2 then
      let s = txt_fun (brother_label conf lev1) in
      Output.print_string conf (Utf8.capitalize_fst (Util.translate_eval s))
    else if lev1 = 2 && lev2 = 1 then
      let s = txt_fun (transl_nth conf "an uncle/an aunt" 4) in
      Output.print_string conf (Utf8.capitalize_fst (Util.translate_eval s))
    else if lev1 = 3 && lev2 = 1 then
      let s = txt_fun (transl_nth conf "a great-uncle/a great-aunt" 4) in
      Output.print_string conf (Utf8.capitalize_fst (Util.translate_eval s))
    else if lev1 = 1 && lev2 = 2 then
      let s = txt_fun (transl_nth conf "a nephew/a niece" 4) in
      Output.print_string conf (Utf8.capitalize_fst (Util.translate_eval s))
    else if lev1 = 1 && lev2 = 3 then
      let s = txt_fun (transl_nth conf "a great-nephew/a great-niece" 4) in
      Output.print_string conf (Utf8.capitalize_fst (Util.translate_eval s))
    else
      Output.printf conf "%s %d / %s %d" (Utf8.capitalize_fst (transl conf "ancestors")) lev1
        (Utf8.capitalize_fst (transl conf "descendants")) lev2
  in
  let max_cnt =
    try int_of_string (List.assoc "max_cousins" conf.base_env) with
      Not_found | Failure _ -> default_max_cnt
  in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Output.print_string conf "<div>\n";
  (*include_templ conf "cousins_tools";*)
  Output.print_string conf "<h3>\n";
  title false;
  Output.print_string conf "</h3>\n";
  Output.print_string conf "</div>\n";
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  let (cnt, cnt_sp) = CousinsCount.print_cousins_lev conf base max_cnt p
    lev1 lev2 true Perso.print_sosa
  in
  Output.print_string conf "<div>\n";
  Output.print_string conf "<p>\n";
  let (cnt2, cnt2_sp) = CousinsCount.print_cousins_lev conf base max_cnt p
    lev1 lev2 false Perso.print_sosa
  in
  if cnt >= max_cnt then Output.print_string conf "etc...\n"
  else if cnt > 1 then
    Output.printf conf "%s%s %d (%d) %s " (Utf8.capitalize_fst (transl conf "total"))
      (Util.transl conf ":") cnt cnt2
			(Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1));
  if p_getenv conf.env "spouse" = Some "on" then
    Output.printf conf " %s %d (%d) %s.\n" (transl conf "and") cnt_sp cnt2_sp
      (Util.translate_eval ("@(c)" ^ transl_nth conf "spouse/spouses" 1))
  else Output.printf conf ".\n" ;
  Output.print_string conf "</p>\n";
  Output.print_string conf "</div>\n";
  Hutil.trailer conf

let print_anniv conf base p dead_people level =
  let module S = Map.Make (struct type t = iper let compare = compare end) in
  let s_mem x m = try let _ = S.find x m in true with Not_found -> false in
  let rec insert_desc set up_sosa down_br n ip =
    if s_mem ip set then set
    else
      let set = S.add ip (up_sosa, down_br) set in
      if n = 0 then set
      else
        let u = get_family (pget conf base ip) in
        let down_br = ip :: down_br in
        let rec loop set i =
          if i = Array.length u then set
          else
            let chil = get_children (foi base u.(i)) in
            let set =
              let rec loop set i =
                if i = Array.length chil then set
                else
                  let set =
                    insert_desc set up_sosa down_br (n - 1) chil.(i)
                  in
                  loop set (i + 1)
              in
              loop set 0
            in
            loop set (i + 1)
        in
        loop set 0
  in
  let set =
    let module P =
      Pqueue.Make
        (struct
          type t = iper * int * int
          let leq (_, lev1, _) (_, lev2, _) = lev1 <= lev2
        end)
    in
    let a = P.add (get_iper p, 0, 1) P.empty in
    let rec loop set a =
      if P.is_empty a then set
      else
        let ((ip, n, up_sosa), a) = P.take a in
        let set = insert_desc set up_sosa [] (n + 3) ip in
        if n >= level then set
        else
          let a =
            match get_parents (pget conf base ip) with
              Some ifam ->
              let cpl = foi base ifam in
              let n = n + 1 in
              let up_sosa = 2 * up_sosa in
              let a = P.add (get_father cpl, n, up_sosa) a in
              P.add (get_mother cpl, n, up_sosa + 1) a
            | None -> a
          in
          loop set a
    in
    loop S.empty a
  in
  let set =
    S.fold
      (fun ip (up_sosa, down_br) set ->
         let u = get_family (pget conf base ip) in
         let set = S.add ip (up_sosa, down_br, None) set in
         if Array.length u = 0 then set
         else
           let rec loop set i =
             if i = Array.length u then set
             else
               let cpl = foi base u.(i) in
               let c = Gutil.spouse ip cpl in
               loop (S.add c (up_sosa, down_br, Some ip) set) (i + 1)
           in
           loop set 0)
      set S.empty
  in
  let txt_of (up_sosa, down_br, spouse) conf base c =
    "<a href=\"" ^ commd conf ^ "m=RL&" ^ acces_n conf base "1" p ^ "&b1=" ^
    string_of_int up_sosa ^ "&" ^
    acces_n conf base "2"
      (match spouse with
         Some ip -> pget conf base ip
       | _ -> c) ^
    "&b2=" ^ string_of_int (sosa_of_persons conf base down_br) ^
    (match spouse with
       Some _ -> "&" ^ acces_n conf base "4" c
     | _ -> "") ^
    "&spouse=on\">" ^ person_title_text conf base c ^ "</a>"
  in
  let f_scan =
    let list = ref (S.fold (fun ip b list -> (ip, b) :: list) set []) in
    fun () ->
      match !list with
        (x, b) :: l -> list := l; pget conf base x, txt_of b
      | [] -> raise Not_found
  in
  let mode () =
    Output.print_string conf "<input type=\"hidden\" name=\"m\" value=\"C\">\n";
    Output.printf conf "<input type=\"hidden\" name=\"i\" value=\"%s\">\n"
      (string_of_iper (get_iper p));
    Output.printf conf "<input type=\"hidden\" name=\"t\" value=\"%s\">\n"
      (if dead_people then "AD" else "AN")
  in
  match p_getint conf.env "v" with
    Some i -> BirthdayDisplay.gen_print conf base i f_scan dead_people
  | _ ->
    if dead_people then BirthdayDisplay.gen_print_menu_dead conf base f_scan mode
    else BirthdayDisplay.gen_print_menu_birth conf base f_scan mode

let cousmenu_print = Perso.interp_templ "cousmenu"

let print conf base p =
  let max_lev =
    try int_of_string (List.assoc "max_cousins_level" conf.base_env) with
      Not_found | Failure _ -> Perso.default_max_cousin_lev
  in
  match (p_getint conf.env "v1", p_getint conf.env "v2", p_getenv conf.env "t") with
  | (Some 1, Some 1, _) | (Some 0, _, _)  | (_, Some 0, _)->
    Perso.interp_templ "cousins" conf base p
  | (Some lev1, _, _) ->
    let lev1 = min (max 1 lev1) max_lev in
    let lev2 =
      match p_getint conf.env "v2" with
        Some lev2 -> min (max 1 lev2) max_lev
      | None -> lev1
    in
    print_cousins conf base p lev1 lev2
  | (_, _, Some (("AN" | "AD") as t)) when conf.wizard || conf.friend ->
    print_anniv conf base p (t = "AD") max_lev
  | _ ->
    cousmenu_print conf base p

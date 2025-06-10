(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
open ChangeChildren
module Driver = Geneweb_db.Driver

let print_child_person conf base p =
  let var = Adef.encoded ("c" ^ Driver.Iper.to_string (Driver.get_iper p)) in
  let first_name =
    match p_getenv conf.env ((var :> string) ^ "_first_name") with
    | Some v -> v
    | None -> Driver.p_first_name base p
  in
  let surname =
    match p_getenv conf.env ((var :> string) ^ "_surname") with
    | Some v -> v
    | None -> Driver.p_surname base p
  in
  let occ =
    match p_getint conf.env ((var :> string) ^ "_occ") with
    | Some i -> i
    | None -> Driver.get_occ p
  in
  Output.print_sstring conf {|<table class="m1-2"><tbody><tr align="|};
  Output.print_sstring conf conf.left;
  Output.print_sstring conf {|"><td>|};
  Output.print_sstring conf {|<label for="|};
  Output.print_string conf var;
  Output.print_sstring conf {|_fn" class="mx-2 mb-0">|};
  transl_nth conf "first name/first names" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf {|</label></td><td colspan="3">|};
  Output.print_sstring conf {|<input name=\"|};
  Output.print_string conf var;
  Output.print_sstring conf
    {|_first_name" class="form-control" size="23" maxlength="200" id="|};
  Output.print_string conf var;
  Output.print_sstring conf {|_fn" value="|};
  Output.print_string conf (Util.escape_html first_name);
  Output.print_sstring conf {|">|};
  Output.print_sstring conf {|</td><td align="|};
  Output.print_sstring conf conf.right;
  Output.print_sstring conf {|"><label for="|};
  Output.print_string conf var;
  Output.print_sstring conf {|_occ" class="mx-2 mb-0">|};
  transl conf "number" |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf {|</label></td><td>|};
  Output.print_sstring conf {|<input class="form-control" id="|};
  Output.print_string conf var;
  Output.print_sstring conf {|_occ" name="|};
  Output.print_string conf var;
  Output.print_sstring conf {|occ" size="5" maxlength="8"|};
  Output.print_sstring conf
    (if occ = 0 then "" else {| value="|} ^ string_of_int occ ^ {|"|});
  Output.print_sstring conf {|</td></tr><tr align="|};
  Output.print_sstring conf conf.left;
  Output.print_sstring conf {|"><td>|};
  Output.print_sstring conf {|<label for="|};
  Output.print_string conf var;
  Output.print_sstring conf {|_sn" class="mx-2 mb-0">|};
  transl_nth conf "surname/surnames" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf {|</label></td><td colspan="5">|};
  Output.print_sstring conf {|<input name="|};
  Output.print_string conf var;
  Output.print_sstring conf
    {|_surname" class="form-control" size="40" maxlength="200" id="|};
  Output.print_string conf var;
  Output.print_sstring conf {|_sn" value="|};
  Output.print_string conf (Util.escape_html surname);
  Output.print_sstring conf {|">|};
  Output.print_sstring conf {|</td></tr></tbody></table>|}

let print_children conf base ipl =
  Output.print_sstring conf "<ul>\n";
  List.iter
    (fun ip ->
      let p = Driver.poi base ip in
      Output.print_sstring conf {|<li class="mt-3"><span class="ml-2">|};
      Output.print_string conf
        (reference conf base p (gen_person_text conf base p));
      Output.print_string conf (DateDisplay.short_dates_text conf base p);
      Output.print_sstring conf {|</span>|};
      print_child_person conf base p;
      Output.print_sstring conf "</li>")
    ipl;
  Output.print_sstring conf "</ul>"

let print_change conf base p =
  let title _ =
    transl conf "change children's names"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  let children = Driver.children_of_p base p in
  let digest = digest_children base children in
  Hutil.header conf title;
  Output.print_sstring conf
    (Format.sprintf {|<h2>%s %s</h2>
<form method="post" action="%s">|}
       (let s : Adef.safe_string = gen_person_text conf base p in
        let r : Adef.safe_string = reference conf base p s in
        Util.transl_a_of_b conf "" (r :> string) (s :> string))
       (DateDisplay.short_dates_text conf base p :> string)
       (conf.command :> string));
  Util.hidden_env conf;
  Util.hidden_input_s conf "ip" (Driver.Iper.to_string (Driver.get_iper p));
  Util.hidden_input_s conf "digest" digest;
  Util.hidden_input_s conf "m" "CHG_CHN_OK";
  print_children conf base children;
  Output.print_sstring conf
    {|<button type="submit" class="btn btn-primary btn-lg ml-5 mb-2">|};
  transl_nth conf "validate/delete" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "</button></form>";
  Hutil.trailer conf

let print conf base =
  match p_getenv conf.env "ip" with
  | Some i ->
      let p = Driver.poi base (Driver.Iper.of_string i) in
      print_change conf base p
  | _ -> Hutil.incorrect_request conf

let print_children_list conf base u =
  Output.print_sstring conf "<h4>";
  transl_nth conf "child/children" 1
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "</h4><p><ul>";
  Array.iter
    (fun ifam ->
      let des = Driver.foi base ifam in
      Array.iter
        (fun ip ->
          let p = Driver.poi base ip in
          Output.print_sstring conf "<li>";
          gen_person_text conf base p
          |> reference conf base p |> Output.print_string conf;
          Output.print_string conf (DateDisplay.short_dates_text conf base p))
        (Driver.get_children des))
    (Driver.get_family u);
  Output.print_sstring conf "</ul>"

let print_change_done conf base p =
  let title _ =
    transl conf "children's names changed"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  gen_person_text conf base p
  |> reference conf base p |> Output.print_string conf;
  Output.print_string conf (DateDisplay.short_dates_text conf base p);
  print_children_list conf base p;
  Hutil.trailer conf

let print_conflict conf base ip_var p =
  let var = "c" ^ Driver.Iper.to_string ip_var in
  Update.print_create_conflict conf base p var

let error_person conf err =
  let title _ =
    transl conf "error" |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.rheader conf title;
  Output.printf conf "%s\n" (Utf8.capitalize_fst err);
  Hutil.trailer conf;
  raise
  @@ Update.ModErr
       (Update.UERR (__FILE__ ^ " " ^ string_of_int __LINE__ |> Adef.safe))

let print_update_child conf base =
  match p_getenv conf.env "m" with
  | Some "CHG_CHN_OK" -> print conf base
  | _ -> Hutil.incorrect_request conf

let print_change_ok conf base p =
  let ipl = Driver.children_of_p base p in
  let parent_surname = Driver.p_surname base p in
  let redisp = Option.is_some (p_getenv conf.env "return") in
  if redisp then print_update_child conf base
  else (
    check_digest conf (digest_children base ipl);
    let changed =
      try change_children conf base parent_surname ipl with
      | ChangeChildrenConflict (p, p') ->
          print_conflict conf base (Driver.get_iper p) p'
      | FirstNameMissing _ ->
          error_person conf (transl conf "first name missing")
    in
    Util.commit_patches conf base;
    let changed =
      U_Change_children_name
        (Util.string_gen_person base (Driver.gen_person_of_person p), changed)
    in
    History.record conf base changed "cn";
    print_change_done conf base p)

let print_ok o_conf base =
  let conf = Update.update_conf o_conf in
  match p_getenv conf.env "ip" with
  | Some i ->
      let p = Driver.poi base (Driver.Iper.of_string i) in
      print_change_ok conf base p
  | _ -> Hutil.incorrect_request conf

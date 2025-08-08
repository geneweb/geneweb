open Config

let t conf ?(c = 1) l =
  let s = Util.transl conf l in
  if c <> 0 then Utf8.capitalize_fst s else s

let tn conf ?(c = 1) l n =
  let s = Util.transl_nth conf l n in
  if c <> 0 then Utf8.capitalize_fst s else s

module TranslCache = struct
  let cache = Hashtbl.create 32

  let get conf key =
    match Hashtbl.find_opt cache (conf.lang, key) with
    | Some v -> v
    | None ->
        let v = t conf key in
        Hashtbl.add cache (conf.lang, key) v;
        v
end

type dict_info = {
  dict_type : CheckData.dict_type;
  transl_key : string;
  url_param : string;
  icon : string;
  form_param : string;
}

module DictInfo = struct
  let all =
    [
      {
        dict_type = CheckData.Fnames;
        transl_key = "first name/first names";
        url_param = "fn";
        icon = "child";
        form_param = "d_fn";
      };
      {
        dict_type = CheckData.Snames;
        transl_key = "surname/surnames";
        url_param = "sn";
        icon = "user";
        form_param = "d_sn";
      };
      {
        dict_type = CheckData.Places;
        transl_key = "place/places";
        url_param = "place";
        icon = "map-marker-alt";
        form_param = "d_pl";
      };
      {
        dict_type = CheckData.PubNames;
        transl_key = "public name/public names";
        url_param = "pubn";
        icon = "bullhorn";
        form_param = "d_pn";
      };
      {
        dict_type = CheckData.Qualifiers;
        transl_key = "qualifier/qualifiers";
        url_param = "qual";
        icon = "comment";
        form_param = "d_qu";
      };
      {
        dict_type = CheckData.Aliases;
        transl_key = "alias/aliases";
        url_param = "alias";
        icon = "mask";
        form_param = "d_al";
      };
      {
        dict_type = CheckData.Occupation;
        transl_key = "occupation/occupations";
        url_param = "occu";
        icon = "briefcase";
        form_param = "d_oc";
      };
      {
        dict_type = CheckData.Titles;
        transl_key = "title/titles";
        url_param = "title";
        icon = "crown";
        form_param = "d_ti";
      };
      {
        dict_type = CheckData.Estates;
        transl_key = "domain/domains";
        url_param = "domain";
        icon = "building";
        form_param = "d_es";
      };
      {
        dict_type = CheckData.Sources;
        transl_key = "source/sources";
        url_param = "src";
        icon = "box-archive";
        form_param = "d_sr";
      };
    ]

  let find_by_type d = List.find (fun i -> i.dict_type = d) all
  let get_name d = (find_by_type d).transl_key
  let get_url_param d = (find_by_type d).url_param
end

let get_sel_dicts conf =
  DictInfo.all
  |> List.filter (fun info -> Util.p_getenv conf.env info.form_param = Some "1")
  |> List.map (fun info -> info.dict_type)

type error_info = {
  error_type : CheckData.error_type;
  transl_key : string;
  form_param : string;
  css_class : string;
}

module ErrorInfo = struct
  let all =
    [
      {
        error_type = CheckData.InvisibleCharacters;
        transl_key = "chk_data error invisible characters";
        form_param = "e_ic";
        css_class = "ic";
      };
      {
        error_type = CheckData.BadCapitalization;
        transl_key = "chk_data error bad capitalization";
        form_param = "e_bc";
        css_class = "bc";
      };
      {
        error_type = CheckData.MultipleSpaces;
        transl_key = "chk_data error multiple spaces";
        form_param = "e_ms";
        css_class = "ms";
      };
      {
        error_type = CheckData.NonBreakingSpace;
        transl_key = "chk_data error non-breaking spaces";
        form_param = "e_nb";
        css_class = "nb";
      };
    ]

  let find_by_type e = List.find (fun i -> i.error_type = e) all
  let get_name e = (find_by_type e).transl_key
end

let get_sel_err_types conf =
  ErrorInfo.all
  |> List.filter (fun info -> Util.p_getenv conf.env info.form_param = Some "1")
  |> List.map (fun info -> info.error_type)

let error_type_name conf err_type = t conf (ErrorInfo.get_name err_type)

let get_max_results conf =
  let config_max =
    match List.assoc_opt "chk_data_max_results" conf.base_env with
    | Some "" -> None
    | Some s -> (
        try
          let n = int_of_string s in
          if n > 0 then Some n else Some 150
        with _ -> Some 150)
    | None -> Some 150
  in

  let form_max =
    match Util.p_getenv conf.env "max" with
    | Some "" -> None
    | Some s -> (
        try
          let n = int_of_string s in
          if n > 0 then Some n else None
        with _ -> None)
    | None -> None
  in

  match (config_max, form_max) with
  | Some c, Some f -> Some (min c f)
  | Some c, None -> Some c
  | None, Some f -> Some f
  | None, None -> None

let render_error_entry_fast conf base dict_param istr s error_type =
  let book_title = TranslCache.get conf "view person record" in
  let fix_title = TranslCache.get conf "fix error automatically" in
  let highlighted, url_mod, url_chk, entry, visible =
    CheckData.make_error_html conf base dict_param istr s error_type
  in
  let visibility = if visible then "" else " style=\"visibility:hidden\"" in
  Printf.sprintf
    {|    <div class="err" data-ori="%s">
      <a href="%s" class="btn btn-primary" 
         title="%s"><i class="fa fa-book"></i></a>
      <button onclick="return false;">%s</button>
      <a href="%s" target="_blank" class="s2 btn btn-info"
         title="%s"%s><i class="fa fa-check"></i></a>
    </div>
|}
    entry url_mod book_title highlighted url_chk fix_title visibility

let render_dict_section_streaming conf base dict filtrd_entries sel_err_types =
  let dict_title = tn conf (DictInfo.get_name dict) 1 in
  let dict_param = DictInfo.get_url_param dict in
  Output.printf conf
    {|<div class="card mt-3">
      <div class="card-header">
        <h3 class="font-weight bold mb-0">%s</h3>
      </div>
      <div class="card-body">|}
    dict_title;
  List.iter
    (fun error_type ->
      let entries_for_error =
        List.filter
          (fun (_, _, errors) -> List.mem error_type errors)
          filtrd_entries
      in
      if entries_for_error <> [] then (
        let error_count = List.length entries_for_error in
        let error_name = error_type_name conf error_type in
        Output.printf conf {|<h4>%s (%d)</h4><div class="list-group">|}
          error_name error_count;
        List.iter
          (fun (istr, s, _) ->
            Output.print_sstring conf
              (render_error_entry_fast conf base dict_param istr s error_type))
          entries_for_error;
        Output.print_sstring conf "</div>"))
    sel_err_types;
  Output.print_sstring conf "</div></div>"

let render_error_section conf base dict entries_for_error error_type =
  let dict_param = DictInfo.get_url_param dict in
  let error_count = List.length entries_for_error in
  let error_name = error_type_name conf error_type in
  let entries_html =
    entries_for_error
    |> List.map (fun (istr, s, _) ->
           render_error_entry_fast conf base dict_param istr s error_type)
    |> String.concat ""
  in
  Printf.sprintf {|<h4>%s (%d)</h4><div class="list-group">%s</div>|} error_name
    error_count entries_html

let render_dict_section conf base dict filtrd_entries sel_err_types =
  let dict_title = tn conf (DictInfo.get_name dict) 1 in
  let sections_html =
    sel_err_types
    |> List.filter_map (fun error_type ->
           let entries_for_error =
             List.filter_map
               (fun (istr, s, errors) ->
                 if List.mem error_type errors then Some (istr, s, errors)
                 else None)
               filtrd_entries
           in
           if entries_for_error <> [] then
             Some
               (render_error_section conf base dict entries_for_error error_type)
           else None)
    |> String.concat ""
  in
  Printf.sprintf
    {|<div class="card mt-3">
        <div class="card-header">
          <h3 class="font-weight bold mb-0">%s</h3>
        </div>
        <div class="card-body">%s</div>
      </div>|}
    dict_title sections_html

let render_missing_cache_warning conf missing_caches =
  if missing_caches = [] then ""
  else
    let buf = Buffer.create 512 in
    Printf.bprintf buf
      {|<div class="alert alert-danger mt-2">
          <i class="fa fa-exclamation-triangle mr-2"></i>%s%s |}
      (tn conf "chk_data cache file not found" 0)
      (t conf ":");
    Buffer.add_string buf
      (missing_caches |> List.rev
      |> List.map CheckData.dict_to_cache_name
      |> String.concat ", ");
    Printf.bprintf buf
      {|.<br>%s%s <b><code class="user-select-all">.\gw\cache_file -bd ..\bases -all %s</code></b></div>|}
      (tn conf "chk_data cache file not found" 1)
      (t conf ":") conf.bname;
    Buffer.contents buf

let render_summary_message conf total_entries max_results =
  match total_entries with
  | 0 ->
      Printf.sprintf {|<div class="alert alert-info mt-3">%s</div>|}
        (t conf "no match")
  | n ->
      let errors_msg = Util.ftransl conf "chk_data %d errors found" in
      let limit_reached =
        match max_results with Some max -> n >= max | None -> false
      in
      let limit_msg =
        if limit_reached then " (" ^ t ~c:0 conf "chk_data limit reached" ^ ")"
        else ""
      in
      Printf.sprintf
        {|<div class="alert alert-success mt-3"><strong>%s</strong>%s</div>|}
        (Printf.sprintf errors_msg n)
        limit_msg

let render_dict_checkboxes_two_columns conf selected_dicts =
  let buf = Buffer.create 1024 in
  let rec split_at n lst =
    match (n, lst) with
    | 0, _ | _, [] -> ([], lst)
    | n, h :: t ->
        let left, right = split_at (n - 1) t in
        (h :: left, right)
  in
  let left_col, right_col = split_at 5 DictInfo.all in
  let render_group infos =
    List.iter
      (fun (info : dict_info) ->
        let id =
          "dict-"
          ^ String.sub info.form_param 2 (String.length info.form_param - 2)
        in
        let checked = List.mem info.dict_type selected_dicts in
        Printf.bprintf buf
          {|<div class="form-check">
              <input class="form-check-input" type="checkbox" 
                     name="%s" id="%s" value="1"%s>
              <label class="form-check-label" for="%s">
                <i class="fa fa-%s fa-fw mr-1"></i>%s
              </label>
            </div>
|}
          info.form_param id
          (if checked then " checked" else "")
          id info.icon
          (tn conf info.transl_key 1))
      infos
  in
  Buffer.add_string buf {|<div class="w-auto mr-4">
|};
  render_group left_col;
  Buffer.add_string buf {|</div>
<div class="w-auto">
|};
  render_group right_col;
  Buffer.add_string buf {|</div>|};
  Buffer.contents buf

let render_error_checkboxes conf sel_err_types =
  let buf = Buffer.create 512 in
  List.iter
    (fun info ->
      let id = "err-" ^ info.css_class in
      let checked = List.mem info.error_type sel_err_types in
      Printf.bprintf buf
        {|<div class="form-check form-check-inline">
         <input class="form-check-input" type="checkbox" name="%s" id="%s" value="1"%s>
         <label class="form-check-label" for="%s">%s</label>
       </div>|}
        info.form_param id
        (if checked then " checked" else "")
        id
        (tn conf info.transl_key 1))
    ErrorInfo.all;
  Buffer.contents buf

let display_results conf base dicts sel_err_types max_results =
  let use_cache = Util.p_getenv conf.env "nocache" <> Some "1" in
  let total_entries_found = ref 0 in
  let missing_caches = ref [] in
  let sections_buffer = Buffer.create 4096 in
  let entries_buffer = ref [] in
  let process_dict dict =
    if
      match max_results with
      | Some max -> !total_entries_found < max
      | None -> true
    then (
      let remaining =
        match max_results with
        | Some max -> Some (max - !total_entries_found)
        | None -> None
      in
      let entries =
        CheckData.collect_all_errors_with_cache ~max_results:remaining
          ~sel_err_types conf base dict
      in
      if use_cache && not (CheckData.cache_file_exists conf dict) then
        missing_caches := dict :: !missing_caches;
      if entries <> [] then (
        entries_buffer := (dict, entries) :: !entries_buffer;
        total_entries_found := !total_entries_found + List.length entries))
  in
  List.iter process_dict dicts;
  List.iter
    (fun (dict, entries) ->
      if List.length entries > 250 then
        render_dict_section_streaming conf base dict entries sel_err_types
      else
        Buffer.add_string sections_buffer
          (render_dict_section conf base dict entries sel_err_types))
    (List.rev !entries_buffer);
  Output.print_sstring conf (Buffer.contents sections_buffer);
  if !missing_caches <> [] then
    Output.print_sstring conf
      (render_missing_cache_warning conf !missing_caches);
  if dicts <> [] then
    Output.print_sstring conf
      (render_summary_message conf !total_entries_found max_results)

type params = {
  selected_dicts : CheckData.dict_type list;
  sel_err_types : CheckData.error_type list;
  max_results : int option;
  form_max : int option;
  config_max : int option;
  nocache_checked : bool;
  is_roglo : bool;
}

let print conf base =
  let title _ = Output.print_sstring conf (t conf "data typographic checker") in
  let params =
    {
      selected_dicts = get_sel_dicts conf;
      sel_err_types = get_sel_err_types conf;
      max_results = get_max_results conf;
      form_max =
        (match Util.p_getenv conf.env "max" with
        | Some s -> ( try Some (int_of_string s) with _ -> None)
        | None -> None);
      config_max =
        (match List.assoc_opt "chk_data_max_results" conf.base_env with
        | Some "" -> None
        | Some s -> ( try Some (int_of_string s) with _ -> Some 150)
        | None -> Some 150);
      nocache_checked = Util.p_getenv conf.env "nocache" = Some "1";
      is_roglo =
        (try List.assoc "roglo" conf.base_env = "yes" with Not_found -> false);
    }
  in
  Hutil.header conf title;
  Hutil.HtmlBuffer.wrap_measured conf (fun conf ->
      Util.print_loading_overlay conf ();
      Output.printf conf
        {|<div class="container">
  <form method="get" action="%s" class="mt-2" id="chk-data-form">
    <input type="hidden" name="m" value="CHK_DATA">
      <div class="d-flex justify-content-center mb-3">
        <div class="card">
          <div class="card-header">
            <h5 class="mb-0">%s (%d)</h5>
          </div>
          <div class="card-body d-flex align-items-start flex-column">
            <div class="d-flex flex-row mb-2">%s</div>
            <div class="mt-auto align-self-center">
              <button type="button" class="btn btn-sm btn-outline-primary"
                      data-action="toggle-dicts">
                <i class="fa fa-check-square mr-1"></i>%s
              </button>
            </div>
          </div>
        </div>
        <div class="card mx-3">
          <div class="card-header">
            <h5 class="mb-0">%s (%d)</h5>
          </div>
          <div class="card-body d-flex align-items-start flex-column">
            %s
            <div class="mt-auto align-self-center">
              <button type="button" class="btn btn-sm btn-outline-primary"
                      data-action="toggle-errors">
                <i class="fa fa-check-square mr-1"></i>%s
              </button>
            </div>
          </div>
        </div>
        <div class="card">
          <div class="card-header">
            <h5 class="mb-0"><i class="fa fa-cog mr-1"></i>%s</h5>
          </div>
          <div class="card-body pb-1">|}
        (Util.commd conf :> string)
        (t conf "chk_data books to check")
        (List.length params.selected_dicts)
        (render_dict_checkboxes_two_columns conf params.selected_dicts)
        (t conf "toggle all")
        (t conf "chk_data error types")
        (List.length params.sel_err_types)
        (render_error_checkboxes conf params.sel_err_types)
        (t conf "toggle all") (t conf "options");
      if not params.is_roglo then
        Output.printf conf
          {|
          <div class="form-check mb-1">
            <input class="form-check-input" type="checkbox" name="nocache"
                   id="use-db" value="1"%s>
            <label class="form-check-label" for="use-db">
              <i class="fa fa-database mr-2"></i>%s
            </label>
          </div>|}
          (if params.nocache_checked then " checked" else "")
          (tn conf "chk_data use database/cache" 0);
      Output.printf conf
        {|
       <div class="form-group mb-0">
         <label for="max-results" class="mb-0">%s%s</label>
         <input type="number" class="form-control" name="max" id="max-results"
                min="1" step="1" value="%s"%s>%s
       </div>|}
        (t conf "chk_data max results")
        (t conf ":")
        (match params.form_max with Some n -> string_of_int n | None -> "")
        (match params.config_max with
        | Some n -> Printf.sprintf {| max="%d"|} n
        | None -> "")
        (match params.config_max with
        | Some c ->
            Printf.sprintf {|<small class="ml-1 text-muted">%s</small>|}
              (Utf8.capitalize_fst
                 (Printf.sprintf
                    (Util.ftransl conf "chk_data limited to %d results")
                    c))
        | None -> "");
      Output.printf conf
        {|
          <div class="text-center py-1 px-3 my-1">
            <button type="submit" class="btn btn-primary w-100"
                    data-action="validate-submit">
              <i class="fa fa-search mr-2"></i>%s</button>
          </div>
        </div>
      </div>
   </div>
  </form>|}
        (t conf "chk_data check data");
      if params.selected_dicts <> [] && params.sel_err_types <> [] then (
        let cache_index = if params.nocache_checked then 1 else 2 in
        Output.printf conf
          {|<div class="alert alert-info mt-3">
           <i class="fa fa-database mr-2"></i>%s
         </div><div id="cd" data-ok-title="%s">|}
          (tn conf "chk_data use database/cache" cache_index)
          (tn conf "validate/delete" 0);
        if params.selected_dicts <> [] && params.sel_err_types <> [] then
          display_results conf base params.selected_dicts params.sel_err_types
            params.max_results);
      Output.print_sstring conf "</div></div>";
      (* Hutil.trailer_with_extra_js conf "checkdata.js|overlay";*)
      (*Util.print_loading_overlay_js conf;*)
      Hutil.trailer conf)

type chk_result =
  | Success of string * string * bool (* before, after, cache_updated *)
  | Error of string

let perform_check_modification conf base =
  let k =
    Geneweb_db.Driver.Istr.of_string (List.assoc "k" conf.env :> string)
  in
  let s =
    Option.fold ~none:"" ~some:(fun x -> x) (Util.p_getenv conf.env "s")
  in
  let s2 =
    Option.fold ~none:"" ~some:Util.only_printable
      (Util.p_getenv conf.env "s2")
  in
  let dict_param = Util.p_getenv conf.env "d" in
  let k_actual = Geneweb_db.Driver.sou base k in
  if k_actual <> s then Error (t conf "modification failed") (* k/s mismatch *)
  else if s = s2 then Error (t conf "no modification")
  else
    let _ = Geneweb_db.Driver.replace_string base s s2 in
    Util.commit_patches conf base;
    let cache_updated =
      match dict_param with
      | Some d -> (
          let dict_type_opt =
            List.find_opt (fun info -> info.url_param = d) DictInfo.all
          in
          match dict_type_opt with
          | Some info -> CheckData.update_cache_entry conf info.dict_type k s2
          | None -> false)
      | None -> (
          match CheckData.find_dict_type_for_istr conf k with
          | Some dict_type -> CheckData.update_cache_entry conf dict_type k s2
          | None -> false)
    in
    Success (s, s2, cache_updated)

let print_chk_ok_json conf base =
  try
    match perform_check_modification conf base with
    | Success (before, after, cache_updated) ->
        let success_msg = t conf "modification successful" in
        let cache_msg =
          if cache_updated then " (" ^ t ~c:0 conf "cache updated" ^ ")" else ""
        in
        let validated_msg = t conf "validated" in
        let json =
          `Assoc
            [
              ("success", `Bool true);
              ("message", `String (success_msg ^ cache_msg));
              ("cache_updated", `Bool cache_updated);
              ("before", `String before);
              ("after", `String after);
              ("validated_title", `String validated_msg);
            ]
        in
        Yojson.Basic.to_string json
    | Error msg ->
        let json =
          `Assoc
            [
              ("success", `Bool false);
              ("message", `String msg);
              ("cache_updated", `Bool false);
              ("before", `String "");
              ("after", `String "");
            ]
        in
        Yojson.Basic.to_string json
  with _ ->
    let json =
      `Assoc
        [
          ("success", `Bool false);
          ("message", `String (t conf "modification failed"));
          ("cache_updated", `Bool false);
          ("before", `String "");
          ("after", `String "");
        ]
    in
    Yojson.Basic.to_string json

let print_status_message conf ~success ~icon ~title_msg ~alert_msg =
  let text_class, alert_class =
    if success then ("text-success", "alert-success")
    else ("text-danger", "alert-danger")
  in
  Output.printf conf "<h3 class=\"%s\">%s %s</h3>\n" text_class icon title_msg;
  Output.printf conf "<div class=\"alert %s\">%s</div>\n" alert_class alert_msg

let print_chk_ok_html conf base =
  try
    match perform_check_modification conf base with
    | Success (_, _, cache_updated) ->
        let title _ =
          "✓ " ^ t conf ~c:0 "modification successful"
          |> Output.print_sstring conf
        in
        Hutil.header conf title;
        Hutil.HtmlBuffer.wrap_measured conf (fun conf ->
            let msg = t conf ~c:0 "modification successful" in
            print_status_message conf ~success:true ~icon:"✓" ~title_msg:msg
              ~alert_msg:msg;
            if cache_updated then
              Output.printf conf "<div class=\"text-muted\">%s</div>\n"
                (t conf "cache updated"));
        Hutil.trailer conf
    | Error msg ->
        let title _ =
          "✗ " ^ t conf ~c:0 "modification failed" |> Output.print_sstring conf
        in
        Hutil.header conf title;
        Hutil.HtmlBuffer.wrap_measured conf (fun conf ->
            print_status_message conf ~success:false ~icon:"✗"
              ~title_msg:(t conf ~c:0 "modification failed")
              ~alert_msg:msg)
  with _ ->
    let title _ =
      "✗ " ^ t conf ~c:0 "modification failed" |> Output.print_sstring conf
    in
    Hutil.header conf title;
    Hutil.HtmlBuffer.wrap_measured conf (fun conf ->
        let msg = t conf ~c:0 "modification failed" in
        print_status_message conf ~success:false ~icon:"✗" ~title_msg:msg
          ~alert_msg:msg);
    Hutil.trailer conf

let print_chk_ok conf base =
  match Util.p_getenv conf.env "ajax" with
  | Some _ ->
      Output.header conf "application/json";
      Output.print_sstring conf (print_chk_ok_json conf base)
  | _ -> print_chk_ok_html conf base

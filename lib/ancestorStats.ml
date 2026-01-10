open Config

let get_max_implex_level conf =
  let bmil =
    match List.assoc_opt "max_implex_lev" conf.base_env with
    | Some s -> ( try max 1 (int_of_string s) with _ -> 20)
    | None -> 20
  in
  match List.assoc_opt "mil" conf.env with
  | Some s -> (
      let s = Adef.as_string s in
      try max 1 (int_of_string s) with _ -> bmil)
  | None -> bmil

let compute_span base ancestors =
  let min_y = ref max_int in
  let max_y = ref min_int in
  let update y =
    if y < !min_y then min_y := y;
    if y > !max_y then max_y := y
  in
  List.iter
    (fun (ip, _, _, _) ->
      let p = Geneweb_db.Driver.poi base ip in
      (match Date.od_of_cdate (Geneweb_db.Driver.get_birth p) with
      | Some (Def.Dgreg (dg, _)) -> update dg.Def.year
      | _ -> ());
      match Geneweb_db.Driver.get_death p with
      | Def.Death (_, cd) -> (
          match Date.od_of_cdate cd with
          | Some (Def.Dgreg (dg, _)) -> update dg.Def.year
          | _ -> ())
      | _ -> ())
    ancestors;
  if !min_y = max_int then "-" else Printf.sprintf "%d-%d" !min_y !max_y

let theoretical_max_cumul level =
  if level >= 61 then 0 else (1 lsl (level + 1)) - 2

let theoretical_max_level level = if level >= 61 then 0 else 1 lsl level

let compute_percent num denom =
  if denom <= 0 then 0 else min 100 (100 * num / denom)

let format_si_number n level =
  let levelp = level + 1 in
  if level < 14 then string_of_int n
  else
    let dd = levelp / 10 in
    let divisor =
      let rec pow10 acc k = if k <= 0 then acc else pow10 (acc * 10) (k - 1) in
      pow10 1 (3 * dd)
    in
    let reduced = n / divisor in
    let suffix =
      match dd with
      | 1 -> "k"
      | 2 -> "M"
      | 3 -> "G"
      | 4 -> "T"
      | 5 -> "P"
      | 6 -> "E"
      | 7 -> "Z"
      | 8 | 9 -> "Y"
      | _ -> ">10³⁰"
    in
    Printf.sprintf "%d %s" reduced suffix

let format_gradient_cumul pct_pp pct_uc pct_pc =
  if pct_uc = 0 && pct_pc = 0 then ""
  else
    Printf.sprintf
      " style=\\\"background:linear-gradient(90deg,rgba(155,200,255,.5) \
       %d%%,rgba(210,230,255,.5) %d%%,rgba(210,230,255,.5) \
       %d%%,rgba(255,210,180,.5) %d%%,rgba(255,210,180,.5) \
       %d%%,rgba(255,255,255,0) %d%%)\\\""
      pct_pp pct_pp pct_uc pct_uc pct_pc pct_pc

let format_gradient_level pct_ul pct_pl =
  if pct_ul = 0 && pct_pl = 0 then ""
  else
    Printf.sprintf
      " style=\\\"background:linear-gradient(90deg,rgba(180,200,220,.5) \
       %d%%,rgba(255,210,180,.5) %d%%,rgba(255,210,180,.5) \
       %d%%,rgba(255,255,255,0) %d%%)\\\""
      pct_ul pct_ul pct_pl pct_pl

let format_pp theo pct_u pct_p level =
  if level >= 61 || theo = 0 then ""
  else
    let theo_str = format_si_number theo level in
    if pct_u = pct_p then
      if pct_u = 0 then Printf.sprintf "/%s (%d ‰)" theo_str (pct_u * 10)
      else Printf.sprintf "/%s (%d %%)" theo_str pct_u
    else if pct_u = 0 then
      Printf.sprintf "/%s (%d-%d ‰)" theo_str (pct_u * 10) (pct_p * 10)
    else Printf.sprintf "/%s (%d-%d %%)" theo_str pct_u pct_p

let escape_json_string s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let gen_text conf level =
  let nth = Util.transl_nth conf "nth (generation)" level in
  let s = Printf.sprintf (Util.ftransl conf "the %s generation") nth in
  escape_json_string (" " ^ s)

let compute_json conf base p =
  let mil = get_max_implex_level conf in
  let ip = Geneweb_db.Driver.get_iper p in
  let buf = Buffer.create 16384 in
  let data_buf = Buffer.create 16384 in
  let seen_cumul = Hashtbl.create 4096 in
  let cumul_paths = ref 0 in
  let prev_pct_uc = ref 0 in
  let first = ref true in
  let current_level = Hashtbl.create 4096 in
  let next_level = Hashtbl.create 4096 in
  Hashtbl.add current_level ip 1;
  let i = ref 0 in
  let max_lv = ref 0 in
  while Hashtbl.length current_level > 0 do
    incr i;
    Hashtbl.clear next_level;
    let seen_at_level = Hashtbl.create 128 in
    let ul = ref 0 in
    let pl = ref 0 in
    let min_y = ref max_int in
    let max_y = ref min_int in
    Hashtbl.iter
      (fun pip cnt ->
        let pp = Geneweb_db.Driver.poi base pip in
        match Geneweb_db.Driver.get_parents pp with
        | Some ifam ->
            let fam = Geneweb_db.Driver.foi base ifam in
            let fath = Geneweb_db.Driver.get_father fam in
            let moth = Geneweb_db.Driver.get_mother fam in
            List.iter
              (fun parent_ip ->
                if !i < mil then pl := !pl + cnt;
                let is_new_at_level =
                  not (Hashtbl.mem seen_at_level parent_ip)
                in
                if is_new_at_level then begin
                  Hashtbl.add seen_at_level parent_ip ();
                  let parent = Geneweb_db.Driver.poi base parent_ip in
                  (match
                     Date.od_of_cdate (Geneweb_db.Driver.get_birth parent)
                   with
                  | Some (Def.Dgreg (dg, _)) ->
                      if dg.Def.year < !min_y then min_y := dg.Def.year;
                      if dg.Def.year > !max_y then max_y := dg.Def.year
                  | _ -> ());
                  match Geneweb_db.Driver.get_death parent with
                  | Def.Death (_, cd) -> (
                      match Date.od_of_cdate cd with
                      | Some (Def.Dgreg (dg, _)) ->
                          if dg.Def.year < !min_y then min_y := dg.Def.year;
                          if dg.Def.year > !max_y then max_y := dg.Def.year
                      | _ -> ())
                  | _ -> ()
                end;
                let is_new = not (Hashtbl.mem seen_cumul parent_ip) in
                if is_new then begin
                  incr ul;
                  Hashtbl.add seen_cumul parent_ip ()
                end;
                if !i < mil then begin
                  let prev =
                    try Hashtbl.find next_level parent_ip with Not_found -> 0
                  in
                  Hashtbl.replace next_level parent_ip (prev + cnt)
                end
                else if is_new_at_level then
                  Hashtbl.replace next_level parent_ip 1)
              [ fath; moth ]
        | None -> ())
      current_level;
    let ul = !ul in
    let pl = !pl in
    let ul_level = Hashtbl.length seen_at_level in
    if ul_level > 0 then begin
      max_lv := !i;
      if !i < mil then cumul_paths := !cumul_paths + pl;
      let uc = Hashtbl.length seen_cumul in
      let pc = !cumul_paths in
      let ic = if !i < mil then pc - uc else 0 in
      let il_local = if !i < mil then pl - ul_level else 0 in
      let il_global = ul_level - ul in
      let il = il_local + il_global in
      let theo_c = theoretical_max_cumul !i in
      let theo_l = theoretical_max_level !i in
      let pct_uc = compute_percent uc theo_c in
      let pct_pc = if !i < mil then compute_percent pc theo_c else pct_uc in
      let pct_ul = compute_percent ul theo_l in
      let pct_pl = if !i < mil then compute_percent pl theo_l else pct_ul in
      let pct_pp = !prev_pct_uc in
      prev_pct_uc := pct_uc;
      let txt = gen_text conf !i in
      let per =
        if !min_y = max_int then "-" else Printf.sprintf "%d-%d" !min_y !max_y
      in
      if not !first then Buffer.add_char data_buf ',';
      first := false;
      Buffer.add_string data_buf "{\"v\":";
      Buffer.add_string data_buf (string_of_int !i);
      Buffer.add_string data_buf ",\"t\":\"";
      Buffer.add_string data_buf txt;
      Buffer.add_string data_buf "\",\"lex\":\"";
      Buffer.add_string data_buf txt;
      Buffer.add_string data_buf "\",\"noa\":\"";
      Buffer.add_string data_buf (string_of_int uc);
      Buffer.add_string data_buf "\",\"noa_l\":\"";
      Buffer.add_string data_buf (string_of_int ul);
      Buffer.add_string data_buf "\",\"path\":\"";
      if ic > 0 then (
        Buffer.add_char data_buf '+';
        Buffer.add_string data_buf (string_of_int ic));
      Buffer.add_string data_buf "\",\"path_l\":\"";
      if il > 0 then (
        Buffer.add_char data_buf '+';
        Buffer.add_string data_buf (string_of_int il));
      Buffer.add_string data_buf "\",\"per\":\"";
      Buffer.add_string data_buf per;
      Buffer.add_string data_buf "\",\"sty\":\"";
      Buffer.add_string data_buf (format_gradient_cumul pct_pp pct_uc pct_pc);
      Buffer.add_string data_buf "\",\"sty_l\":\"";
      Buffer.add_string data_buf (format_gradient_level pct_ul pct_pl);
      Buffer.add_string data_buf "\",\"pp\":\"";
      Buffer.add_string data_buf
        (escape_json_string (format_pp theo_c pct_uc pct_pc !i));
      Buffer.add_string data_buf "\",\"pp_l\":\"";
      Buffer.add_string data_buf
        (escape_json_string (format_pp theo_l pct_ul pct_pl !i));
      Buffer.add_string data_buf "\"}"
    end;
    Hashtbl.reset current_level;
    Hashtbl.iter (fun k v -> Hashtbl.add current_level k v) next_level
  done;
  Buffer.add_string buf "{\"max\":";
  Buffer.add_string buf (string_of_int !max_lv);
  Buffer.add_string buf ",\"data\":[";
  Buffer.add_buffer buf data_buf;
  Buffer.add_string buf "]}";
  Buffer.contents buf

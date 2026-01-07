(* ancStatsDisplay.ml - Ancestor statistics JSON for Select2 ancmenu display *)

module Percent : sig
  type t

  val of_int : int -> t
  val to_int : t -> int
end = struct
  type t = int

  let of_int x = max 0 (min 100 x)
  let to_int x = x
end

type gradient_cumul = {
  gc_prev : Percent.t;
  gc_unique : Percent.t;
  gc_paths : Percent.t;
}

type gradient_level = { gl_unique : Percent.t; gl_paths : Percent.t }

let make_gradient_cumul ~prev ~unique ~paths =
  let p = Percent.of_int prev in
  let u = Percent.of_int (max prev unique) in
  let s = Percent.of_int (max (max prev unique) paths) in
  { gc_prev = p; gc_unique = u; gc_paths = s }

let make_gradient_level ~unique ~paths =
  let u = Percent.of_int unique in
  let s = Percent.of_int (max unique paths) in
  { gl_unique = u; gl_paths = s }

(* Theoretical maximum ancestors at generation n:
   - cumulated: 2^(n+1) - 2 (all ancestors from gen 1 to n)
   - at level:  2^n (ancestors at exactly gen n) *)
let max_ancestors_cumul gen = if gen >= 61 then 0 else (1 lsl (gen + 1)) - 2
let max_ancestors_level gen = if gen >= 61 then 0 else 1 lsl gen
let percent num denom = if denom <= 0 then 0 else min 100 (100 * num / denom)

(* SI notation for large theoretical counts: k, M, G, T, P, E, Z, Y *)
let format_si n gen =
  if gen < 14 then string_of_int n
  else
    let decade = (gen + 1) / 10 in
    let rec pow10 acc k = if k <= 0 then acc else pow10 (acc * 10) (k - 1) in
    let divisor = pow10 1 (3 * decade) in
    let suffix =
      match decade with
      | 1 -> "k"
      | 2 -> "M"
      | 3 -> "G"
      | 4 -> "T"
      | 5 -> "P"
      | 6 -> "E"
      | 7 -> "Z"
      | _ -> "Y"
    in
    Printf.sprintf "%d %s" (n / divisor) suffix

(* Percentage tooltip: "/theoretical (pct%)" or "/theoretical (pct_u-pct_p%)" *)
let format_pct_tooltip theo pct_unique pct_paths gen =
  if gen >= 61 || theo = 0 then ""
  else
    let theo_str = format_si theo gen in
    if pct_unique = pct_paths then
      if pct_unique = 0 then
        Printf.sprintf "/%s (%d ‰)" theo_str (pct_unique * 10)
      else Printf.sprintf "/%s (%d %%)" theo_str pct_unique
    else if pct_unique = 0 then
      Printf.sprintf "/%s (%d-%d ‰)" theo_str (pct_unique * 10) (pct_paths * 10)
    else Printf.sprintf "/%s (%d-%d %%)" theo_str pct_unique pct_paths

let escape_json s =
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

let generation_label conf gen =
  let nth = Util.transl_nth conf "nth (generation)" gen in
  let s = Printf.sprintf (Util.ftransl conf "the %s generation") nth in
  escape_json (" " ^ s)

(* Extract birth/death year from person *)
let get_years base parent_ip =
  let p = Geneweb_db.Driver.poi base parent_ip in
  let year_of_cdate cd =
    match Date.od_of_cdate cd with
    | Some (Def.Dgreg (dg, _)) -> Some dg.Def.year
    | _ -> None
  in
  let birth = year_of_cdate (Geneweb_db.Driver.get_birth p) in
  let baptism = year_of_cdate (Geneweb_db.Driver.get_baptism p) in
  let death =
    match Geneweb_db.Driver.get_death p with
    | Def.Death (_, cd) -> year_of_cdate cd
    | _ -> None
  in
  let burial =
    match Geneweb_db.Driver.get_burial p with
    | Def.Buried cd | Def.Cremated cd -> year_of_cdate cd
    | Def.UnknownBurial -> None
  in
  let pick a b = match a with Some _ -> a | None -> b in
  let early = pick birth baptism in
  let late = pick death burial in
  (early, late)

(* JSON schema:
   data[] = {
     v  : generation number
     tt : label
     noa: unique cumulative ancestors
     path: implex
     per: year range
     gc : { prev; unique; paths }  (* cumulative percentages *)
     gl : { unique; paths }        (* level percentages *)
     pp : tooltip cumul
     pp_l : tooltip level }*)

(* Main JSON computation for ancestor menu *)
let compute_json conf base p =
  let root_ip = Geneweb_db.Driver.get_iper p in
  let buf = Buffer.create 16384 in
  let data_buf = Buffer.create 16384 in

  let seen_ever = Hashtbl.create 4096 in
  let total_paths = ref 0 in
  let prev_unique_cumul = ref 0 in

  let curr_gen = Hashtbl.create 4096 in
  let next_gen = Hashtbl.create 4096 in
  Hashtbl.add curr_gen root_ip 1;

  let gen = ref 0 in
  let max_gen = ref 0 in
  let first = ref true in

  while Hashtbl.length curr_gen > 0 do
    incr gen;
    Hashtbl.clear next_gen;

    let seen_this_gen = Hashtbl.create 128 in
    let new_anc = ref 0 in
    let paths_this_gen = ref 0 in
    let min_year = ref max_int in
    let max_year = ref min_int in

    Hashtbl.iter
      (fun child_ip path_count ->
        let child = Geneweb_db.Driver.poi base child_ip in
        match Geneweb_db.Driver.get_parents child with
        | None -> ()
        | Some ifam ->
            let fam = Geneweb_db.Driver.foi base ifam in
            let parents =
              [
                Geneweb_db.Driver.get_father fam;
                Geneweb_db.Driver.get_mother fam;
              ]
            in
            List.iter
              (fun parent_ip ->
                paths_this_gen := !paths_this_gen + path_count;

                if not (Hashtbl.mem seen_this_gen parent_ip) then begin
                  Hashtbl.add seen_this_gen parent_ip ();
                  let birth, death = get_years base parent_ip in
                  let upd = function
                    | Some y ->
                        if y < !min_year then min_year := y;
                        if y > !max_year then max_year := y
                    | None -> ()
                  in
                  upd birth;
                  upd death
                end;

                if not (Hashtbl.mem seen_ever parent_ip) then begin
                  incr new_anc;
                  Hashtbl.add seen_ever parent_ip ()
                end;

                let prev =
                  try Hashtbl.find next_gen parent_ip with Not_found -> 0
                in
                Hashtbl.replace next_gen parent_ip (prev + path_count))
              parents)
      curr_gen;

    let unique_this_gen = Hashtbl.length seen_this_gen in

    if unique_this_gen > 0 then begin
      max_gen := !gen;
      total_paths := !total_paths + !paths_this_gen;

      let unique_cumul = Hashtbl.length seen_ever in
      let paths_cumul = !total_paths in

      let implex_cumul = paths_cumul - unique_cumul in
      let implex_local =
        !paths_this_gen - unique_this_gen + (unique_this_gen - !new_anc)
      in

      let theo_c = max_ancestors_cumul !gen in
      let theo_l = max_ancestors_level !gen in

      let pct_prev = percent !prev_unique_cumul theo_c in
      let pct_uc = percent unique_cumul theo_c in
      let pct_pc = percent paths_cumul theo_c in
      let pct_ul = percent !new_anc theo_l in
      let pct_pl = percent !paths_this_gen theo_l in

      let gc =
        make_gradient_cumul ~prev:pct_prev ~unique:pct_uc ~paths:pct_pc
      in
      let gl = make_gradient_level ~unique:pct_ul ~paths:pct_pl in

      let label = generation_label conf !gen in
      let period =
        if !min_year = max_int then "-"
        else Printf.sprintf "%d-%d" !min_year !max_year
      in

      if not !first then Buffer.add_char data_buf ',';
      first := false;

      Buffer.add_string data_buf "{\"v\":";
      Buffer.add_string data_buf (string_of_int !gen);
      Buffer.add_string data_buf ",\"t\":\"";
      Buffer.add_string data_buf label;
      Buffer.add_string data_buf "\",\"noa\":\"";
      Buffer.add_string data_buf (string_of_int unique_cumul);
      Buffer.add_string data_buf "\",\"noa_l\":\"";
      Buffer.add_string data_buf (string_of_int !new_anc);
      Buffer.add_string data_buf "\",\"path\":\"";
      if implex_cumul > 0 then (
        Buffer.add_char data_buf '+';
        Buffer.add_string data_buf (string_of_int implex_cumul));
      Buffer.add_string data_buf "\",\"path_l\":\"";
      if implex_local > 0 then (
        Buffer.add_char data_buf '+';
        Buffer.add_string data_buf (string_of_int implex_local));
      Buffer.add_string data_buf "\",\"per\":\"";
      Buffer.add_string data_buf period;
      Buffer.add_string data_buf "\"";

      (* gc *)
      Buffer.add_string data_buf ",\"gc\":{\"prev\":";
      Buffer.add_string data_buf (string_of_int (Percent.to_int gc.gc_prev));
      Buffer.add_string data_buf ",\"unique\":";
      Buffer.add_string data_buf (string_of_int (Percent.to_int gc.gc_unique));
      Buffer.add_string data_buf ",\"paths\":";
      Buffer.add_string data_buf (string_of_int (Percent.to_int gc.gc_paths));
      Buffer.add_string data_buf "}";

      (* gl *)
      Buffer.add_string data_buf ",\"gl\":{\"unique\":";
      Buffer.add_string data_buf (string_of_int (Percent.to_int gl.gl_unique));
      Buffer.add_string data_buf ",\"paths\":";
      Buffer.add_string data_buf (string_of_int (Percent.to_int gl.gl_paths));
      Buffer.add_string data_buf "}";

      (* tooltips *)
      Buffer.add_string data_buf ",\"pp\":\"";
      Buffer.add_string data_buf
        (escape_json (format_pct_tooltip theo_c pct_uc pct_pc !gen));
      Buffer.add_string data_buf "\",\"pp_l\":\"";
      Buffer.add_string data_buf
        (escape_json (format_pct_tooltip theo_l pct_ul pct_pl !gen));
      Buffer.add_string data_buf "\"}";

      prev_unique_cumul := unique_cumul
    end;

    Hashtbl.reset curr_gen;
    Hashtbl.iter (fun k v -> Hashtbl.add curr_gen k v) next_gen
  done;

  Buffer.add_string buf "{\"max\":";
  Buffer.add_string buf (string_of_int !max_gen);
  Buffer.add_string buf ",\"data\":[";
  Buffer.add_buffer buf data_buf;
  Buffer.add_string buf "]}";
  Buffer.contents buf

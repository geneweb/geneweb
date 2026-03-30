open Config
open Def
open Util
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

let ipi = Driver.Iper.hash
let year_min = 500
let year_max = 2100
let year_span = year_max - year_min + 1
let age_max = 150
let marriage_age_min = 10
let marriage_age_max = 80
let marriage_age_span = marriage_age_max - marriage_age_min + 1
let children_max = 20
let remarriage_max = 4
let relation_kind_count = 11
let top_n = 20

type acc = {
  mutable n_total : int;
  mutable n_men : int;
  mutable n_women : int;
  mutable n_neuter : int;
  mutable n_alive : int;
  mutable n_dead : int;
  mutable n_unknown_status : int;
  mutable n_noname : int;
  mutable n_isolated : int;
  mutable sum_life_men : int;
  mutable cnt_life_men : int;
  mutable sum_life_women : int;
  mutable cnt_life_women : int;
  age_death_men : int array;
  age_death_women : int array;
  births_by_year : int array;
  deaths_by_year : int array;
  marriages_by_year : int array;
  births_by_month : int array;
  deaths_by_month : int array;
  marriages_by_month : int array;
  marriage_dow : int array;
  age_marriage_men : int array;
  age_marriage_women : int array;
  children_count : int array;
  remarriage : int array;
  life_cent_sum_men : int array;
  life_cent_cnt_men : int array;
  life_cent_sum_women : int array;
  life_cent_cnt_women : int array;
  mutable sum_gap_father : int;
  mutable cnt_gap_father : int;
  mutable sum_gap_mother : int;
  mutable cnt_gap_mother : int;
  mutable n_divorces : int;
  mutable n_separations : int;
  relation_counts : int array;
  surnames : int Driver.Istr.Table.t;
  firstnames_m : int Driver.Istr.Table.t;
  firstnames_f : int Driver.Istr.Table.t;
  occupations : int Driver.Istr.Table.t;
  birth_places : int Driver.Istr.Table.t;
  death_places : int Driver.Istr.Table.t;
  marriage_places : int Driver.Istr.Table.t;
  mutable has_birth_date : int;
  mutable has_birth_place : int;
  mutable has_death_date : int;
  mutable has_death_place : int;
  mutable has_parents : int;
  mutable has_occupation : int;
  mutable has_sources : int;
  mutable has_image : int;
  mutable n_families : int;
  mutable has_fsources : int;
}

let make_acc () =
  {
    n_total = 0;
    n_men = 0;
    n_women = 0;
    n_neuter = 0;
    n_alive = 0;
    n_dead = 0;
    n_unknown_status = 0;
    n_noname = 0;
    n_isolated = 0;
    sum_life_men = 0;
    cnt_life_men = 0;
    sum_life_women = 0;
    cnt_life_women = 0;
    age_death_men = Array.make age_max 0;
    age_death_women = Array.make age_max 0;
    births_by_year = Array.make year_span 0;
    deaths_by_year = Array.make year_span 0;
    marriages_by_year = Array.make year_span 0;
    births_by_month = Array.make 12 0;
    deaths_by_month = Array.make 12 0;
    marriages_by_month = Array.make 12 0;
    marriage_dow = Array.make 7 0;
    age_marriage_men = Array.make marriage_age_span 0;
    age_marriage_women = Array.make marriage_age_span 0;
    children_count = Array.make (children_max + 1) 0;
    remarriage = Array.make (remarriage_max + 1) 0;
    life_cent_sum_men = Array.make 17 0;
    life_cent_cnt_men = Array.make 17 0;
    life_cent_sum_women = Array.make 17 0;
    life_cent_cnt_women = Array.make 17 0;
    sum_gap_father = 0;
    cnt_gap_father = 0;
    sum_gap_mother = 0;
    cnt_gap_mother = 0;
    n_divorces = 0;
    n_separations = 0;
    relation_counts = Array.make relation_kind_count 0;
    surnames = Driver.Istr.Table.create 4096;
    firstnames_m = Driver.Istr.Table.create 4096;
    firstnames_f = Driver.Istr.Table.create 4096;
    occupations = Driver.Istr.Table.create 1024;
    birth_places = Driver.Istr.Table.create 2048;
    death_places = Driver.Istr.Table.create 2048;
    marriage_places = Driver.Istr.Table.create 2048;
    has_birth_date = 0;
    has_birth_place = 0;
    has_death_date = 0;
    has_death_place = 0;
    has_parents = 0;
    has_occupation = 0;
    has_sources = 0;
    has_image = 0;
    n_families = 0;
    has_fsources = 0;
  }

let incr_istr_tbl tbl k =
  if not (Driver.Istr.is_quest k || Driver.Istr.is_empty k) then
    let c = try Driver.Istr.Table.find tbl k with Not_found -> 0 in
    Driver.Istr.Table.replace tbl k (c + 1)

let birth_dmy_greg p = Date.cdate_to_gregorian_dmy_opt (Driver.get_birth p)

let death_dmy_greg p =
  match Driver.get_death p with
  | Death (_, cd) -> Date.cdate_to_gregorian_dmy_opt cd
  | _ -> None

let yi y = y - year_min

let person_pass conf base a by_cache auth_cache =
  Collection.iter
    (fun p ->
      let ip = Driver.get_iper p in
      let auth = authorized_age conf base p in
      Bytes.set auth_cache (ipi ip) (if auth then '\001' else '\000');
      a.n_total <- a.n_total + 1;
      (match Driver.get_sex p with
      | Male -> a.n_men <- a.n_men + 1
      | Female -> a.n_women <- a.n_women + 1
      | Neuter -> a.n_neuter <- a.n_neuter + 1);
      if Util.is_empty_name p then a.n_noname <- a.n_noname + 1;
      let has_parents = Driver.get_parents p <> None in
      let has_family = Array.length (Driver.get_family p) > 0 in
      if (not has_parents) && not has_family then
        a.n_isolated <- a.n_isolated + 1;
      if has_parents then a.has_parents <- a.has_parents + 1;
      (match Driver.get_death p with
      | NotDead -> a.n_alive <- a.n_alive + 1
      | DontKnowIfDead -> a.n_unknown_status <- a.n_unknown_status + 1
      | Death _ | DeadYoung | DeadDontKnowWhen | OfCourseDead ->
          a.n_dead <- a.n_dead + 1);
      let sn = Driver.get_surname p in
      incr_istr_tbl a.surnames sn;
      let fn = Driver.get_first_name p in
      (match Driver.get_sex p with
      | Male -> incr_istr_tbl a.firstnames_m fn
      | Female -> incr_istr_tbl a.firstnames_f fn
      | Neuter -> ());
      let occ = Driver.get_occupation p in
      incr_istr_tbl a.occupations occ;
      let bp = Driver.get_birth_place p in
      incr_istr_tbl a.birth_places bp;
      let dp = Driver.get_death_place p in
      incr_istr_tbl a.death_places dp;
      if not (Driver.Istr.is_empty occ || Driver.Istr.is_quest occ) then
        a.has_occupation <- a.has_occupation + 1;
      if not (Driver.Istr.is_empty bp || Driver.Istr.is_quest bp) then
        a.has_birth_place <- a.has_birth_place + 1;
      if not (Driver.Istr.is_empty dp || Driver.Istr.is_quest dp) then
        a.has_death_place <- a.has_death_place + 1;
      if not (Driver.Istr.is_empty (Driver.get_psources p)) then
        a.has_sources <- a.has_sources + 1;
      if not (Driver.Istr.is_empty (Driver.get_image p)) then
        a.has_image <- a.has_image + 1;
      let nf = Array.length (Driver.get_family p) in
      let ri = min nf remarriage_max in
      a.remarriage.(ri) <- a.remarriage.(ri) + 1;
      let bdmy = birth_dmy_greg p in
      (match bdmy with
      | Some d ->
          by_cache.(ipi ip) <- d.year;
          a.has_birth_date <- a.has_birth_date + 1;
          if auth then begin
            let iy = yi d.year in
            if iy >= 0 && iy < year_span then
              a.births_by_year.(iy) <- a.births_by_year.(iy) + 1;
            if d.month >= 1 && d.month <= 12 then
              a.births_by_month.(d.month - 1) <-
                a.births_by_month.(d.month - 1) + 1
          end
      | None -> ());
      let ddmy = death_dmy_greg p in
      (match ddmy with
      | Some _ -> a.has_death_date <- a.has_death_date + 1
      | None -> ());
      if auth then begin
        (match ddmy with
        | Some d ->
            let iy = yi d.year in
            if iy >= 0 && iy < year_span then
              a.deaths_by_year.(iy) <- a.deaths_by_year.(iy) + 1;
            if d.month >= 1 && d.month <= 12 then
              a.deaths_by_month.(d.month - 1) <-
                a.deaths_by_month.(d.month - 1) + 1
        | None -> ());
        match (bdmy, ddmy) with
        | Some bd, Some dd ->
            let age = dd.year - bd.year in
            let age =
              if
                dd.month > 0 && bd.month > 0
                && (dd.month < bd.month
                   || dd.month = bd.month && dd.day > 0 && bd.day > 0
                      && dd.day < bd.day)
              then age - 1
              else age
            in
            if age >= 0 && age < age_max then
              begin match Driver.get_sex p with
              | Male ->
                  a.age_death_men.(age) <- a.age_death_men.(age) + 1;
                  a.sum_life_men <- a.sum_life_men + age;
                  a.cnt_life_men <- a.cnt_life_men + 1;
                  let ci = (bd.year - year_min) / 100 in
                  if ci >= 0 && ci < 17 then begin
                    a.life_cent_sum_men.(ci) <- a.life_cent_sum_men.(ci) + age;
                    a.life_cent_cnt_men.(ci) <- a.life_cent_cnt_men.(ci) + 1
                  end
              | Female ->
                  a.age_death_women.(age) <- a.age_death_women.(age) + 1;
                  a.sum_life_women <- a.sum_life_women + age;
                  a.cnt_life_women <- a.cnt_life_women + 1;
                  let ci = (bd.year - year_min) / 100 in
                  if ci >= 0 && ci < 17 then begin
                    a.life_cent_sum_women.(ci) <-
                      a.life_cent_sum_women.(ci) + age;
                    a.life_cent_cnt_women.(ci) <- a.life_cent_cnt_women.(ci) + 1
                  end
              | Neuter -> ()
              end
        | _ -> ()
      end)
    (Driver.persons base)

let relation_kind_ord = function
  | Married -> 0
  | NotMarried -> 1
  | Engaged -> 2
  | NoSexesCheckNotMarried -> 3
  | NoMention -> 4
  | NoSexesCheckMarried -> 5
  | MarriageBann -> 6
  | MarriageContract -> 7
  | MarriageLicense -> 8
  | Pacs -> 9
  | Residence -> 10

let family_pass _conf a by_cache auth_cache base =
  Collection.iter
    (fun fam ->
      a.n_families <- a.n_families + 1;
      let fi = ipi (Driver.get_father fam) in
      let mi = ipi (Driver.get_mother fam) in
      let f_auth = Bytes.get auth_cache fi = '\001' in
      let m_auth = Bytes.get auth_cache mi = '\001' in
      let both_auth = f_auth && m_auth in
      let rk = Driver.get_relation fam in
      let ri = relation_kind_ord rk in
      a.relation_counts.(ri) <- a.relation_counts.(ri) + 1;
      (match Driver.get_divorce fam with
      | Divorced _ -> a.n_divorces <- a.n_divorces + 1
      | _ -> ());
      (match Driver.get_separation fam with
      | Separated _ | Separated_old -> a.n_separations <- a.n_separations + 1
      | _ -> ());
      if not (Driver.Istr.is_empty (Driver.get_fsources fam)) then
        a.has_fsources <- a.has_fsources + 1;
      let mp = Driver.get_marriage_place fam in
      incr_istr_tbl a.marriage_places mp;
      let children = Driver.get_children fam in
      let nc = Array.length children in
      let ci = min nc children_max in
      a.children_count.(ci) <- a.children_count.(ci) + 1;
      if both_auth then begin
        let mdmy = Date.cdate_to_gregorian_dmy_opt (Driver.get_marriage fam) in
        (match mdmy with
        | Some d ->
            let iy = yi d.year in
            if iy >= 0 && iy < year_span then
              a.marriages_by_year.(iy) <- a.marriages_by_year.(iy) + 1;
            if d.month >= 1 && d.month <= 12 then
              a.marriages_by_month.(d.month - 1) <-
                a.marriages_by_month.(d.month - 1) + 1;
            if d.day > 0 && d.month > 0 then begin
              let sdn = Date.to_sdn ~from:Dgregorian d in
              a.marriage_dow.(sdn mod 7) <- a.marriage_dow.(sdn mod 7) + 1
            end;
            let my = d.year in
            let fy = by_cache.(fi) in
            if fy > 0 then begin
              let age = my - fy in
              let ai = age - marriage_age_min in
              if ai >= 0 && ai < marriage_age_span then
                a.age_marriage_men.(ai) <- a.age_marriage_men.(ai) + 1
            end;
            let my_ = by_cache.(mi) in
            if my_ > 0 then begin
              let age = d.year - my_ in
              let ai = age - marriage_age_min in
              if ai >= 0 && ai < marriage_age_span then
                a.age_marriage_women.(ai) <- a.age_marriage_women.(ai) + 1
            end
        | None -> ());
        let fy = by_cache.(fi) in
        let my = by_cache.(mi) in
        Array.iter
          (fun cip ->
            let cy = by_cache.(ipi cip) in
            if cy > 0 then begin
              if fy > 0 then begin
                let gap = cy - fy in
                if gap > 0 && gap < 100 then begin
                  a.sum_gap_father <- a.sum_gap_father + gap;
                  a.cnt_gap_father <- a.cnt_gap_father + 1
                end
              end;
              if my > 0 then begin
                let gap = cy - my in
                if gap > 0 && gap < 100 then begin
                  a.sum_gap_mother <- a.sum_gap_mother + gap;
                  a.cnt_gap_mother <- a.cnt_gap_mother + 1
                end
              end
            end)
          children
      end)
    (Driver.families base)

let is_placeholder s =
  s = "" || s = "?" || s = "??" || s = "Xx" || s = "Yy" || String.trim s = ""

let top_entries base n tbl =
  let lst = Driver.Istr.Table.fold (fun k v acc -> (k, v) :: acc) tbl [] in
  let sorted = List.sort (fun (_, a) (_, b) -> compare b a) lst in
  let rec take acc i = function
    | [] -> List.rev acc
    | _ when i >= n -> List.rev acc
    | (k, v) :: rest ->
        let s = Driver.sou base k in
        if is_placeholder s then take acc i rest
        else take ((s, v) :: acc) (i + 1) rest
  in
  take [] 0 sorted

let median_from_histograms h1 h2 =
  let total = ref 0 in
  Array.iter (fun x -> total := !total + x) h1;
  Array.iter (fun x -> total := !total + x) h2;
  if !total = 0 then 0
  else
    let half = !total / 2 in
    let cum = ref 0 in
    let result = ref 0 in
    (try
       for i = 0 to age_max - 1 do
         cum := !cum + h1.(i) + h2.(i);
         if !cum >= half then begin
           result := i;
           raise Exit
         end
       done
     with Exit -> ());
    !result

let json_escape buf s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '"' -> Buffer.add_string buf {|\"|}
    | '\\' -> Buffer.add_string buf {|\\|}
    | '\n' -> Buffer.add_string buf {|\n|}
    | '\r' -> Buffer.add_string buf {|\r|}
    | '\t' -> Buffer.add_string buf {|\t|}
    | c when Char.code c < 0x20 ->
        Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char buf c
  done

let add_str buf s =
  Buffer.add_char buf '"';
  json_escape buf s;
  Buffer.add_char buf '"'

let add_int buf n = Buffer.add_string buf (string_of_int n)

let add_float buf f =
  if Float.is_nan f || Float.is_infinite f then Buffer.add_string buf "null"
  else Buffer.add_string buf (Printf.sprintf "%.1f" f)

let add_int_array buf arr =
  Buffer.add_char buf '[';
  Array.iteri
    (fun i v ->
      if i > 0 then Buffer.add_char buf ',';
      add_int buf v)
    arr;
  Buffer.add_char buf ']'

let add_float_array buf arr =
  Buffer.add_char buf '[';
  Array.iteri
    (fun i v ->
      if i > 0 then Buffer.add_char buf ',';
      add_float buf v)
    arr;
  Buffer.add_char buf ']'

let add_str_array buf arr =
  Buffer.add_char buf '[';
  Array.iteri
    (fun i v ->
      if i > 0 then Buffer.add_char buf ',';
      add_str buf v)
    arr;
  Buffer.add_char buf ']'

let decades_from_year_array arr =
  let first = ref (-1) in
  let last = ref (-1) in
  for i = 0 to year_span - 1 do
    if arr.(i) > 0 then begin
      if !first = -1 then first := i;
      last := i
    end
  done;
  if !first = -1 then ([||], [||])
  else
    let d0 = (!first + year_min) / 10 * 10 in
    let d1 = (!last + year_min) / 10 * 10 in
    let nd = ((d1 - d0) / 10) + 1 in
    let labels = Array.init nd (fun i -> string_of_int (d0 + (i * 10))) in
    let values =
      Array.init nd (fun i ->
          let base_y = d0 + (i * 10) - year_min in
          let s = ref 0 in
          for j = 0 to 9 do
            let idx = base_y + j in
            if idx >= 0 && idx < year_span then s := !s + arr.(idx)
          done;
          !s)
    in
    (labels, values)

let avg n s = if n = 0 then Float.nan else float_of_int s /. float_of_int n
let pct total n = if total = 0 then 0 else ((n * 100) + (total / 2)) / total

let relation_kind_label = function
  | 0 -> "married"
  | 1 -> "not_married"
  | 2 -> "engaged"
  | 3 -> "no_sexes_check_not_married"
  | 4 -> "no_mention"
  | 5 -> "no_sexes_check_married"
  | 6 -> "marriage_bann"
  | 7 -> "marriage_contract"
  | 8 -> "marriage_license"
  | 9 -> "pacs"
  | 10 -> "residence"
  | _ -> "unknown"

let emit_json buf base a =
  let ml, mv = decades_from_year_array a.marriages_by_year in
  let bd_first = ref year_span in
  let bd_last = ref (-1) in
  for i = 0 to year_span - 1 do
    if a.births_by_year.(i) > 0 || a.deaths_by_year.(i) > 0 then begin
      if i < !bd_first then bd_first := i;
      if i > !bd_last then bd_last := i
    end
  done;
  let bd_labels, bd_births, bd_deaths =
    if !bd_last < 0 then ([||], [||], [||])
    else
      let d0 = (!bd_first + year_min) / 10 * 10 in
      let d1 = (!bd_last + year_min) / 10 * 10 in
      let nd = ((d1 - d0) / 10) + 1 in
      let labels = Array.init nd (fun i -> string_of_int (d0 + (i * 10))) in
      let sum arr di =
        let base_y = d0 + (di * 10) - year_min in
        let s = ref 0 in
        for j = 0 to 9 do
          let idx = base_y + j in
          if idx >= 0 && idx < year_span then s := !s + arr.(idx)
        done;
        !s
      in
      let births = Array.init nd (sum a.births_by_year) in
      let deaths = Array.init nd (sum a.deaths_by_year) in
      (labels, births, deaths)
  in
  let median = median_from_histograms a.age_death_men a.age_death_women in
  let n_total_nz = if a.n_total > 0 then a.n_total else 1 in
  Buffer.add_char buf '{';
  Buffer.add_string buf {|"counts":{|};
  Printf.bprintf buf {|"total":%d,"men":%d,"women":%d,"neuter":%d,|} a.n_total
    a.n_men a.n_women a.n_neuter;
  Printf.bprintf buf {|"alive":%d,"dead":%d,"unknown_status":%d,|} a.n_alive
    a.n_dead a.n_unknown_status;
  Printf.bprintf buf {|"noname":%d,"isolated":%d,|} a.n_noname a.n_isolated;
  Printf.bprintf buf {|"families":%d,"divorces":%d,"separations":%d}|}
    a.n_families a.n_divorces a.n_separations;
  Buffer.add_string buf {|,"averages":{|};
  Buffer.add_string buf {|"life_men":|};
  add_float buf (avg a.cnt_life_men a.sum_life_men);
  Buffer.add_string buf {|,"life_women":|};
  add_float buf (avg a.cnt_life_women a.sum_life_women);
  Printf.bprintf buf {|,"median_life":%d|} median;
  Buffer.add_string buf {|,"generation_gap_father":|};
  add_float buf (avg a.cnt_gap_father a.sum_gap_father);
  Buffer.add_string buf {|,"generation_gap_mother":|};
  add_float buf (avg a.cnt_gap_mother a.sum_gap_mother);
  Buffer.add_char buf '}';
  Buffer.add_string buf {|,"completeness":{|};
  Printf.bprintf buf {|"birth_date":%d,"birth_place":%d,|}
    (pct n_total_nz a.has_birth_date)
    (pct n_total_nz a.has_birth_place);
  Printf.bprintf buf {|"death_date":%d,"death_place":%d,|}
    (pct n_total_nz a.has_death_date)
    (pct n_total_nz a.has_death_place);
  Printf.bprintf buf {|"parents":%d,"occupation":%d,|}
    (pct n_total_nz a.has_parents)
    (pct n_total_nz a.has_occupation);
  Printf.bprintf buf {|"sources":%d,"image":%d,|}
    (pct n_total_nz a.has_sources)
    (pct n_total_nz a.has_image);
  Printf.bprintf buf {|"families_sourced":%d}|}
    (pct (max 1 a.n_families) a.has_fsources);
  Buffer.add_string buf {|,"births_deaths_by_decade":{"labels":|};
  add_str_array buf bd_labels;
  Buffer.add_string buf {|,"births":|};
  add_int_array buf bd_births;
  Buffer.add_string buf {|,"deaths":|};
  add_int_array buf bd_deaths;
  Buffer.add_char buf '}';
  Buffer.add_string buf {|,"births_by_month":|};
  add_int_array buf a.births_by_month;
  Buffer.add_string buf {|,"deaths_by_month":|};
  add_int_array buf a.deaths_by_month;
  Buffer.add_string buf {|,"marriages_by_decade":{"labels":|};
  add_str_array buf ml;
  Buffer.add_string buf {|,"values":|};
  add_int_array buf mv;
  Buffer.add_char buf '}';
  Buffer.add_string buf {|,"marriages_by_month":|};
  add_int_array buf a.marriages_by_month;
  Buffer.add_string buf {|,"marriage_day_of_week":|};
  add_int_array buf a.marriage_dow;
  Buffer.add_string buf {|,"age_at_death":{"labels":|};
  let ad_first = ref (-1) in
  let ad_last = ref (-1) in
  for i = 0 to age_max - 1 do
    if a.age_death_men.(i) > 0 || a.age_death_women.(i) > 0 then begin
      if !ad_first = -1 then ad_first := i;
      ad_last := i
    end
  done;
  if !ad_first = -1 then begin
    ad_first := 0;
    ad_last := 0
  end;
  let ad_len = !ad_last - !ad_first + 1 in
  let ad_labels = Array.init ad_len (fun i -> string_of_int (!ad_first + i)) in
  let ad_men = Array.init ad_len (fun i -> a.age_death_men.(!ad_first + i)) in
  let ad_women =
    Array.init ad_len (fun i -> a.age_death_women.(!ad_first + i))
  in
  add_str_array buf ad_labels;
  Buffer.add_string buf {|,"men":|};
  add_int_array buf ad_men;
  Buffer.add_string buf {|,"women":|};
  add_int_array buf ad_women;
  Buffer.add_char buf '}';
  Buffer.add_string buf {|,"life_exp_by_century":{"labels":|};
  let lec_labels = ref [] in
  let lec_men = ref [] in
  let lec_women = ref [] in
  for i = 0 to 16 do
    if a.life_cent_cnt_men.(i) > 0 || a.life_cent_cnt_women.(i) > 0 then begin
      let century = year_min + (i * 100) in
      lec_labels := string_of_int century :: !lec_labels;
      lec_men := avg a.life_cent_cnt_men.(i) a.life_cent_sum_men.(i) :: !lec_men;
      lec_women :=
        avg a.life_cent_cnt_women.(i) a.life_cent_sum_women.(i) :: !lec_women
    end
  done;
  let lec_labels = Array.of_list (List.rev !lec_labels) in
  let lec_men = Array.of_list (List.rev !lec_men) in
  let lec_women = Array.of_list (List.rev !lec_women) in
  add_str_array buf lec_labels;
  Buffer.add_string buf {|,"men":|};
  add_float_array buf lec_men;
  Buffer.add_string buf {|,"women":|};
  add_float_array buf lec_women;
  Buffer.add_char buf '}';
  Buffer.add_string buf {|,"age_at_marriage":{"labels":|};
  let am_first = ref (-1) in
  let am_last = ref (-1) in
  for i = 0 to marriage_age_span - 1 do
    if a.age_marriage_men.(i) > 0 || a.age_marriage_women.(i) > 0 then begin
      if !am_first = -1 then am_first := i;
      am_last := i
    end
  done;
  if !am_first = -1 then begin
    am_first := 0;
    am_last := 0
  end;
  let am_len = !am_last - !am_first + 1 in
  let am_labels =
    Array.init am_len (fun i ->
        string_of_int (marriage_age_min + !am_first + i))
  in
  let am_men =
    Array.init am_len (fun i -> a.age_marriage_men.(!am_first + i))
  in
  let am_women =
    Array.init am_len (fun i -> a.age_marriage_women.(!am_first + i))
  in
  add_str_array buf am_labels;
  Buffer.add_string buf {|,"men":|};
  add_int_array buf am_men;
  Buffer.add_string buf {|,"women":|};
  add_int_array buf am_women;
  Buffer.add_char buf '}';
  Buffer.add_string buf {|,"children_per_family":{"labels":|};
  let cf_last = ref 0 in
  for i = 0 to children_max do
    if a.children_count.(i) > 0 then cf_last := i
  done;
  let cf_len = !cf_last + 1 in
  let cf_labels =
    Array.init cf_len (fun i ->
        if i = children_max then Printf.sprintf "%d+" children_max
        else string_of_int i)
  in
  let cf_values = Array.init cf_len (fun i -> a.children_count.(i)) in
  add_str_array buf cf_labels;
  Buffer.add_string buf {|,"values":|};
  add_int_array buf cf_values;
  Buffer.add_char buf '}';
  Buffer.add_string buf {|,"remarriage":{"labels":|};
  let rm_labels =
    Array.init (remarriage_max + 1) (fun i ->
        if i = remarriage_max then Printf.sprintf "%d+" remarriage_max
        else string_of_int i)
  in
  add_str_array buf rm_labels;
  Buffer.add_string buf {|,"values":|};
  add_int_array buf a.remarriage;
  Buffer.add_char buf '}';
  Buffer.add_string buf {|,"relation_types":{"labels":|};
  let rt_labels =
    Array.init relation_kind_count (fun i -> relation_kind_label i)
  in
  add_str_array buf rt_labels;
  Buffer.add_string buf {|,"values":|};
  add_int_array buf a.relation_counts;
  Buffer.add_char buf '}';
  let emit_top_entries key entries =
    Buffer.add_string buf {|,"|};
    Buffer.add_string buf key;
    Buffer.add_string buf {|":[|};
    List.iteri
      (fun i (name, count) ->
        if i > 0 then Buffer.add_char buf ',';
        Buffer.add_string buf {|{"n":|};
        add_str buf name;
        Printf.bprintf buf {|,"c":%d}|} count)
      entries;
    Buffer.add_char buf ']'
  in
  emit_top_entries "top_surnames" (top_entries base top_n a.surnames);
  emit_top_entries "top_firstnames_m" (top_entries base top_n a.firstnames_m);
  emit_top_entries "top_firstnames_f" (top_entries base top_n a.firstnames_f);
  emit_top_entries "top_occupations" (top_entries base top_n a.occupations);
  emit_top_entries "top_birth_places" (top_entries base top_n a.birth_places);
  emit_top_entries "top_death_places" (top_entries base top_n a.death_places);
  emit_top_entries "top_marriage_places"
    (top_entries base top_n a.marriage_places);
  Buffer.add_char buf '}'

let print_json conf base =
  let np = Driver.nb_of_persons base in
  let a = make_acc () in
  let by_cache = Array.make np 0 in
  let auth_cache = Bytes.make np '\000' in
  person_pass conf base a by_cache auth_cache;
  family_pass conf a by_cache auth_cache base;
  let buf = Buffer.create 16384 in
  emit_json buf base a;
  let charset = if conf.charset = "" then "utf-8" else conf.charset in
  Output.header conf "Content-type: application/json; charset=%s" charset;
  Output.print_sstring conf (Buffer.contents buf);
  Output.flush conf

type 'a env = Vother of 'a

let get_vother = function Vother x -> Some x
let set_vother x = Vother x

let print_page conf =
  let ifun =
    Templ.
      {
        eval_var = (fun _ -> raise Not_found);
        eval_transl = (fun _ -> Templ.eval_transl conf);
        eval_predefined_apply = (fun _ -> raise Not_found);
        get_vother;
        set_vother;
        print_foreach = (fun _ -> raise Not_found);
      }
  in
  Templ.output conf ifun Templ.Env.empty () "statistics"

let print conf base =
  match p_getenv conf.env "json" with
  | Some _ -> print_json conf base
  | None -> print_page conf

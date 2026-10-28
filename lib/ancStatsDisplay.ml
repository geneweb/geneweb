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
  let { Implex.generations; _ } =
    Implex.profile base (Geneweb_db.Driver.get_iper p)
  in
  let data = Buffer.create 16384 in
  let plus n = if n > 0 then "+" ^ string_of_int n else "" in
  let max_gen, _, _ =
    List.fold_left
      (fun (_, unique_prev, paths_prev) g ->
        let { Implex.gen; paths; fresh; period; _ } = g in
        let unique = unique_prev + fresh in
        let paths_cumul = paths_prev + paths in
        let theo_c = max_ancestors_cumul gen in
        let theo_l = max_ancestors_level gen in
        let pct_uc = percent unique theo_c in
        let pct_pc = percent paths_cumul theo_c in
        let pct_ul = percent fresh theo_l in
        let pct_pl = percent paths theo_l in
        let gc =
          make_gradient_cumul
            ~prev:(percent unique_prev theo_c)
            ~unique:pct_uc ~paths:pct_pc
        in
        let gl = make_gradient_level ~unique:pct_ul ~paths:pct_pl in
        if gen > 1 then Buffer.add_char data ',';
        Printf.bprintf data
          {|{"v":%d,"t":"%s","noa":"%d","noa_l":"%d","path":"%s","path_l":"%s","per":"%s","gc":{"prev":%d,"unique":%d,"paths":%d},"gl":{"unique":%d,"paths":%d},"pp":"%s","pp_l":"%s"}|}
          gen
          (generation_label conf gen)
          unique fresh
          (plus (paths_cumul - unique))
          (plus (paths - fresh))
          (match period with
          | Some (a, b) -> Printf.sprintf "%d-%d" a b
          | None -> "-")
          (Percent.to_int gc.gc_prev)
          (Percent.to_int gc.gc_unique)
          (Percent.to_int gc.gc_paths)
          (Percent.to_int gl.gl_unique)
          (Percent.to_int gl.gl_paths)
          (escape_json (format_pct_tooltip theo_c pct_uc pct_pc gen))
          (escape_json (format_pct_tooltip theo_l pct_ul pct_pl gen));
        (gen, unique, paths_cumul))
      (0, 0, 0) generations
  in
  Printf.sprintf {|{"max":%d,"data":[%s]}|} max_gen (Buffer.contents data)

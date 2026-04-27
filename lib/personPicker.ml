open Config
module Driver = Geneweb_db.Driver

let prefix_match prefix s =
  let pl = String.length prefix in
  pl = 0 || (String.length s >= pl && String.sub s 0 pl = prefix)

let pickable conf base p fn sn =
  (not (fn = "?" || sn = "?"))
  && ((not (Util.is_hide_names conf p))
     || Util.is_public conf base p || conf.friend || conf.wizard)

let search_prefix conf base ~limit ~fn_prefix ~sn_prefix =
  if sn_prefix = "" then []
  else
    let spi = Driver.persons_of_surname base in
    let rec iter istr (acc, n) =
      if n >= limit then (acc, n)
      else
        let s = Name.lower (Driver.sou base istr) in
        if not (prefix_match sn_prefix s) then (acc, n)
        else
          let acc, n =
            List.fold_left
              (fun (acc, n) ip ->
                if n >= limit then (acc, n)
                else
                  match Util.pget_opt conf base ip with
                  | None -> (acc, n)
                  | Some p ->
                      let fn = Driver.p_first_name base p in
                      let sn = Driver.p_surname base p in
                      if not (pickable conf base p fn sn) then (acc, n)
                      else if prefix_match fn_prefix (Name.lower fn) then
                        (p :: acc, n + 1)
                      else (acc, n))
              (acc, n) (Driver.spi_find spi istr)
          in
          match Driver.spi_next spi istr with
          | exception Not_found -> (acc, n)
          | next_istr -> iter next_istr (acc, n)
    in
    let acc, _ =
      match Driver.spi_first spi sn_prefix with
      | exception Not_found -> ([], 0)
      | first -> iter first ([], 0)
    in
    List.rev acc

let lookup_exact base fn sn oc =
  match Driver.person_of_key base (Name.lower fn) (Name.lower sn) oc with
  | Some ip -> [ Driver.poi base ip ]
  | None -> []

let person_to_json base p =
  let fn = Driver.p_first_name base p in
  let sn = Driver.p_surname base p in
  let oc = Driver.get_occ p in
  let label = fn ^ " " ^ sn ^ if oc <> 0 then "." ^ string_of_int oc else "" in
  `Assoc
    [
      ("fn", `String fn);
      ("sn", `String sn);
      ("oc", `Int oc);
      ("label", `String label);
    ]

let lookup_print conf base =
  let result =
    match Util.p_getenv conf.env "exact" with
    | Some "1" ->
        let g k = Util.p_getenv conf.env k |> Option.value ~default:"" in
        let oc = try int_of_string (g "oc") with _ -> 0 in
        lookup_exact base (g "fn") (g "sn") oc
    | _ ->
        let q = Util.p_getenv conf.env "q" |> Option.value ~default:"" in
        let limit =
          match Util.p_getenv conf.env "n" with
          | Some s -> ( try max 1 (min 50 (int_of_string s)) with _ -> 20)
          | None -> 20
        in
        let q = Name.lower q in
        let fn_prefix, sn_prefix =
          match String.index_opt q ' ' with
          | None -> ("", q)
          | Some i ->
              (String.sub q 0 i, String.sub q (i + 1) (String.length q - i - 1))
        in
        search_prefix conf base ~limit ~fn_prefix ~sn_prefix
  in
  let json =
    `List (List.map (person_to_json base) result) |> Yojson.Basic.to_string
  in
  Output.status conf Geneweb_http.Code.OK;
  Output.header conf "Content-type: application/json; charset=utf-8";
  Output.print_sstring conf json

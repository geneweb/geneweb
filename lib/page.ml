module Last_name_search = struct
  module Query_params = struct
    type t = {
      last_name : string;
      display_mode : [ `Branch | `List ];
      exact : bool;
    }

    let from_env env =
      Option.map
        (fun last_name ->
          let display_mode =
            match Util.p_getenv env "o" with Some "i" -> `List | _ -> `Branch
          in
          let exact = Util.p_getenv env "t" = Some "A" in
          { last_name; display_mode; exact })
        (Util.p_getenv env "v")

    let canonicalize query_params =
      let last_name =
        ( "v",
          (if query_params.exact then Fun.id else Name.lower)
            query_params.last_name )
      in
      let display_mode =
        match query_params.display_mode with
        | `Branch -> None
        | `List -> Some ("o", "i")
      in
      let exact =
        Ext_option.return_if query_params.exact (fun () -> ("t", "A"))
      in
      let open Ext_list.Infix in
      ("m", "N") @:: display_mode @?: last_name @:: exact @?: []
  end

  let canonical_url ~conf query_params =
    let query = Query_params.canonicalize query_params in
    Canonical_url.make ~conf ~query

  let alternate_url ~conf ~lang query_params =
    let query = Query_params.canonicalize query_params in
    Localized_url.make ~conf ~lang ~query
end

module First_name_search = struct
  module Query_params = struct
    type t = { first_name : string; exact : bool }

    let from_env env =
      Option.map
        (fun first_name ->
          let exact = Util.p_getenv env "t" = Some "A" in
          { first_name; exact })
        (Util.p_getenv env "v")
  end
end

module All_names_list = struct
  module Query_params = struct
    type t = {
      display_mode : [ `Frequency | `Short | `Alphabetical ];
      prefix : string option;
      all : bool;
      at_least : int option;
    }

    let from_env env =
      let display_mode =
        match Util.p_getenv env "tri" with
        | Some "F" -> `Frequency
        | Some "S" -> `Short
        | _ -> `Alphabetical
      in
      let prefix = Util.p_getenv env "k" in
      let all = Util.p_getenv env "o" = Some "A" in
      let at_least = Util.p_getint env "atleast" in
      { display_mode; prefix; all; at_least }
  end
end

module Advanced_search = struct
  module Query_params = struct
    module Event = struct
      type kind = [ `Birth | `Baptism | `Marriage | `Death | `Burial | `Other ]

      type t = {
        place : string option;
        dates : Date.dmy option * Date.dmy option;
      }

      let get_place event = event.place
      let get_dates event = event.dates

      let is_empty { place; dates = date1, date2 } =
        Option.is_none place && Option.is_none date1 && Option.is_none date2
    end

    type t = {
      first_name : string option;
      first_name_search_mode : [ `Exact | `Not_Exact | `Not_Exact_Prefix ];
      surname : string option;
      surname_search_mode : [ `Exact | `Not_Exact | `Not_Exact_Prefix ];
      alias : string option;
      sex : Def.sex;
      married : bool option;
      only_root_ancestors : bool;
      occupation : string option;
      events : (Event.kind * Event.t) list;
      event_search_mode : [ `Or | `And ];
      event_exact_place : bool;
      limit : int option;
    }

    let get_event_info ~event_kind ~get_info query_params =
      Ext_list.find_map
        (fun (event_kind', event) ->
          Ext_option.return_if (event_kind' = event_kind) (fun () ->
              get_info event))
        query_params.events

    let get_event_place ~event_kind query_params =
      Option.join
      @@ get_event_info ~event_kind ~get_info:Event.get_place query_params

    let get_event_dates ~event_kind query_params =
      Option.value ~default:(None, None)
        (get_event_info ~event_kind ~get_info:Event.get_dates query_params)

    let get_number var key env = Util.p_getint env (var ^ "_" ^ key)

    let reconstitute_date_dmy env var =
      match get_number var "yyyy" env with
      | Some y -> (
          match get_number var "mm" env with
          | Some m -> (
              match get_number var "dd" env with
              | Some d ->
                  if d >= 1 && d <= 31 && m >= 1 && m <= 12 then
                    Some
                      Date.
                        { day = d; month = m; year = y; prec = Sure; delta = 0 }
                  else None
              | None ->
                  if m >= 1 && m <= 12 then
                    Some
                      { day = 0; month = m; year = y; prec = Sure; delta = 0 }
                  else None)
          | None ->
              Some { day = 0; month = 0; year = y; prec = Sure; delta = 0 })
      | None -> None

    let sex_of_string = function "M" -> Def.Male | "F" -> Female | _ -> Neuter

    let get_search_type gets =
      match gets "search_type" with "OR" -> `Or | _ -> `And

    let get_name_search_mode gets key =
      let key_pfx = key ^ "_prefix" in
      let value_pfx = gets key_pfx in
      let value = gets key in
      if value = "on" then `Exact
      else if value_pfx = "on" then `Not_Exact_Prefix
      else if value = "pfx" then `Not_Exact_Prefix
      else `Not_Exact

    let are_empty query_params =
      let personal_info_is_empty () =
        let sex_is_empty : Def.sex -> _ = function
          | Male | Female -> false
          | Neuter -> true
        in
        Option.is_none query_params.first_name
        && Option.is_none query_params.surname
        && Option.is_none query_params.alias
        && sex_is_empty query_params.sex
        && Option.is_none query_params.married
        && (not query_params.only_root_ancestors)
        && Option.is_none query_params.occupation
      in
      let events_are_empty () =
        List.for_all Event.is_empty (List.map snd query_params.events)
      in
      personal_info_is_empty () && events_are_empty ()

    let from_env env =
      let query_params =
        let hs = Hashtbl.create 73 in
        let hd = Hashtbl.create 73 in
        let getd x =
          match Hashtbl.find_opt hd x with
          | Some v -> v
          | None ->
              let v =
                ( reconstitute_date_dmy env (x ^ "1"),
                  reconstitute_date_dmy env (x ^ "2") )
              in
              Hashtbl.add hd x v;
              v
        in
        let gets x =
          match Hashtbl.find_opt hs x with
          | Some v -> v
          | None ->
              let v =
                match Util.p_getenv env x with Some v -> v | None -> ""
              in
              Hashtbl.add hs x v;
              v
        in
        let gets_opt x =
          let value = gets x in
          Ext_option.return_if (value <> "") (fun () -> value)
        in
        let married =
          match gets "married" with
          | "Y" -> Some true
          | "N" -> Some false
          | _ -> None
        in
        let event_search_mode = get_search_type gets in
        let events =
          let event_name = function
            | `Birth -> "birth"
            | `Baptism -> "bapt"
            | `Marriage -> "marriage"
            | `Death -> "death"
            | `Burial -> "burial"
            | `Other -> "other_events"
          in
          let all_event_kinds =
            [ `Birth; `Baptism; `Marriage; `Death; `Burial; `Other ]
          in
          match event_search_mode with
          | `Or ->
              let make =
                let event =
                  let event : Event.t =
                    { place = gets_opt "place"; dates = getd "date" }
                  in
                  Ext_option.return_if
                    (not @@ Event.is_empty event)
                    (fun () -> event)
                in
                fun event_kind ->
                  Option.bind event (fun event ->
                      Ext_option.return_if
                        ("on" = gets ("event_" ^ event_name event_kind))
                        (fun () -> (event_kind, event)))
              in
              List.filter_map make all_event_kinds
          | `And ->
              let make event_kind =
                let event : Event.t =
                  let event_name = event_name event_kind in
                  {
                    place = gets_opt @@ Printf.sprintf "%s_place" event_name;
                    dates = getd @@ Printf.sprintf "%s_date" event_name;
                  }
                in
                Ext_option.return_if
                  (not @@ Event.is_empty event)
                  (fun () -> (event_kind, event))
              in
              List.filter_map make all_event_kinds
        in
        {
          first_name = gets_opt "first_name";
          first_name_search_mode = get_name_search_mode gets "exact_first_name";
          surname = gets_opt "surname";
          surname_search_mode = get_name_search_mode gets "exact_surname";
          alias = gets_opt "alias_pubname_qualifiers";
          sex = gets "sex" |> sex_of_string;
          married;
          only_root_ancestors = "on" = gets "sosa_filter";
          occupation = gets_opt "occu";
          events;
          event_search_mode;
          event_exact_place = "on" = gets "exact_place";
          limit = Util.p_getint env "max";
        }
      in
      Ext_option.return_if
        (not @@ are_empty query_params)
        (fun () -> query_params)
  end
end

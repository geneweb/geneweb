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
  end

  let url_query (query_params : Query_params.t) =
    let last_name =
      ( "v",
        (if query_params.exact then Fun.id else Name.lower)
          query_params.last_name )
    in
    let display_mode, exact =
      match query_params.display_mode with
      | `Branch ->
          (None, Ext_option.return_if query_params.exact (fun () -> ("t", "A")))
      | `List -> (Some ("o", "i"), None)
    in
    let open Ext_list.Infix in
    ("m", "N") @:: last_name @:: exact @?: display_mode @?: []

  let canonical_url ~conf query_params =
    let query = url_query query_params in
    Canonical_url.make ~conf ~query

  let alternate_url ~conf ~lang query_params =
    let query = url_query query_params in
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

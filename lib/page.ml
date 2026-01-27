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

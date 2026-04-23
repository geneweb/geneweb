module Last_name_search : sig
  module Query_params : sig
    type t = private {
      last_name : string;
      display_mode : [ `Branch | `List ];
      exact : bool;
    }

    val from_env : Config.env -> t option
  end

  val canonical_url : conf:Config.Trimmed.t -> Query_params.t -> Canonical_url.t

  val alternate_url :
    conf:Config.Trimmed.t -> lang:Lang.t -> Query_params.t -> Localized_url.t
end

module First_name_search : sig
  module Query_params : sig
    type t = private { first_name : string; exact : bool }

    val from_env : Config.env -> t option
  end
end

module All_names_list : sig
  module Query_params : sig
    type t = private {
      display_mode : [ `Frequency | `Short | `Alphabetical ];
      prefix : string option;
      all : bool;
      at_least : int option;
    }

    val from_env : Config.env -> t
  end
end

module Advanced_search : sig
  module Query_params : sig
    module Event : sig
      type kind = [ `Birth | `Baptism | `Marriage | `Death | `Burial | `Other ]

      type t = private {
        place : string option;
        dates : Date.dmy option * Date.dmy option;
      }
    end

    type t = private {
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

    val get_event_place : event_kind:Event.kind -> t -> string option

    val get_event_dates :
      event_kind:Event.kind -> t -> Date.dmy option * Date.dmy option

    val from_env : Config.env -> t option
  end
end

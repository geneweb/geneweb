module Last_name_search : sig
  module Query_params : sig
    type t = private {
      last_name : string;
      display_mode : [ `Branch | `List ];
      exact : bool;
    }

    val from_env : Config.env -> t option
  end
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

(* Copyright (c) 1998-2007 INRIA *)

let max_cousin_level conf base p =
  let default_max_cousin_lvl = 6 in
  let max_lvl =
    Option.value ~default:default_max_cousin_lvl
      (Option.bind
         (List.assoc_opt "max_cousins_level" conf.Config.base_env)
         int_of_string_opt)
  in
  Util.max_ancestor_level conf base (Gwdb.get_iper p) max_lvl + 1

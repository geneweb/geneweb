#ifdef API

open Geneweb
open Config
open Gwdb
open Plugin_lia_lib

module GWD = Gwd_lib

let ns = "lia"

let () =
  Gwd_lib.GwdPlugin.register_se ~ns begin fun _ _ _ ->
    GWPARAM_ITL.get_father := Lia_lib.get_father ;
    GWPARAM_ITL.get_mother := Lia_lib.get_mother ;
    GWPARAM_ITL.get_person := Lia_lib.get_person ;
    GWPARAM_ITL.get_father' := Lia_lib.get_father' ;
    GWPARAM_ITL.get_mother' := Lia_lib.get_mother' ;
    GWPARAM_ITL.init_cache := Lia_lib.init_cache ;
    GWPARAM_ITL.max_ancestor_level := Lia_lib.max_ancestor_level ;
    GWPARAM_ITL.max_descendant_level := Lia_lib.max_descendant_level ;
    GWPARAM_ITL.tree_generation_list := Lia_lib.tree_generation_list ;
    GWPARAM_ITL.get_family := Lia_lib.get_family ;
    GWPARAM_ITL.get_families := Lia_lib.get_families ;
    GWPARAM_ITL.get_children_of_parents := Lia_lib.get_children_of_parents ;
    GWPARAM_ITL.get_children := Lia_lib.get_children ;
    GWPARAM_ITL.get_children' := Lia_lib.get_children' ;
    GWPARAM_ITL.has_children := Lia_lib.has_children ;
    GWPARAM_ITL.has_family_correspondance := Lia_lib.has_family_correspondance ;
    GWPARAM_ITL.has_parents_link := Lia_lib.has_parents_link ;
    GWPARAM_ITL.nb_children := Lia_lib.nb_children ;
    GWPARAM_ITL.nb_families := Lia_lib.nb_families ;
  end

let w_base =
  let none conf =
    if conf.bname = "" then Plugin_api_lib.Api_util.print_error conf `bad_request ""
    else Plugin_api_lib.Api_util.print_error conf `not_found ""
  in
  GWD.Request.w_base ~none

let () =
  let aux fn _assets conf base =
    fn { conf with api_mode = true } base ; true
  in
  GWD.GwdPlugin.register ~ns
    [ ( "API_LINK_TREE"
      , aux @@ w_base @@ Lia_api_link.print_link_tree)
    ]

#endif

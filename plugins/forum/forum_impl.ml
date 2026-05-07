open Geneweb
open Config
module Registration = Geneweb_register.Registration
module Sites = Geneweb_plugins_sites.Sites.Sites

let wrapper fn conf base =
  if List.assoc_opt "disable_forum" conf.base_env <> Some "yes" then
    fn conf base
  else Hutil.incorrect_request conf;
  true

let w_base =
  let none conf =
    Hutil.incorrect_request conf;
    true
  in
  Gwd_lib.Request.w_base ~none

let ( // ) = Filename.concat

let () =
  Secure.add_assets @@ (List.hd Sites.assets // "forum");
  Registration.register ~name:"forum" []
    [
      ("FORUM", w_base @@ wrapper ForumDisplay.print);
      ("FORUM_ADD", w_base @@ wrapper ForumDisplay.print_add);
      ("FORUM_ADD_OK", w_base @@ wrapper ForumDisplay.print_add_ok);
      ("FORUM_DEL", w_base @@ wrapper ForumDisplay.print_del);
      ("FORUM_P_P", w_base @@ wrapper ForumDisplay.print_access_switch);
      ("FORUM_SEARCH", w_base @@ wrapper ForumDisplay.print_search);
      ("FORUM_VAL", w_base @@ wrapper ForumDisplay.print_valid);
      ("FORUM_VIEW", w_base @@ wrapper ForumDisplay.print);
    ]

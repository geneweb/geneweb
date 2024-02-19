module Forum = struct
  #include "forum.ml"
end
module ForumDisplay = struct
  #include "forumDisplay.ml"
end

open Geneweb
open Config

let ns = "forum"

let wrapper fn conf base =
  if List.assoc_opt "disable_forum" conf.base_env <> Some "yes" then
    fn conf base
  else Hutil.incorrect_request conf;
  true

let w_base =
  let none conf =
    Hutil.incorrect_request conf ;
    true
  in
  Gwd_lib.Request.w_base ~none

let () =
  Secure.add_assets !Gwd_lib.GwdPlugin.assets ;
  Gwd_lib.GwdPlugin.register ~ns
    [ "FORUM", (fun _assets -> w_base @@ wrapper ForumDisplay.print)
    ; "FORUM_ADD", (fun _assets -> w_base @@ wrapper ForumDisplay.print_add)
    ; "FORUM_ADD_OK", (fun _assets -> w_base @@ wrapper ForumDisplay.print_add_ok)
    ; "FORUM_DEL", (fun _assets -> w_base @@ wrapper ForumDisplay.print_del)
    ; "FORUM_P_P", (fun _assets -> w_base @@ wrapper ForumDisplay.print_access_switch)
    ; "FORUM_SEARCH", (fun _assets -> w_base @@ wrapper ForumDisplay.print_search)
    ; "FORUM_VAL", (fun _assets -> w_base @@ wrapper ForumDisplay.print_valid)
    ; "FORUM_VIEW", (fun _assets -> w_base @@ wrapper ForumDisplay.print)
    ]

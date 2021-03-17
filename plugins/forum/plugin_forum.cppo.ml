module Forum = struct
  #include "forum.ml"
end
module ForumDisplay = struct
  #include "forumDisplay.ml"
end

open Geneweb
open Config

let ns = "forum"

let wrapper fn _ conf base =
  match base with
  | Some b when List.assoc_opt "disable_forum" conf.base_env <> Some "yes" ->
    fn conf b ;
    true
  | _ ->
    Hutil.incorrect_request conf ;
    true

let () =
  Gwd_lib.GwdPlugin.register ~ns
    [ "FORUM", wrapper ForumDisplay.print
    ; "FORUM_ADD", wrapper ForumDisplay.print_add
    ; "FORUM_ADD_OK", wrapper ForumDisplay.print_add_ok
    ; "FORUM_DEL", wrapper ForumDisplay.print_del
    ; "FORUM_P_P", wrapper ForumDisplay.print_access_switch
    ; "FORUM_SEARCH", wrapper ForumDisplay.print_search
    ; "FORUM_VAL", wrapper ForumDisplay.print_valid
    ; "FORUM_VIEW", wrapper ForumDisplay.print
    ]

type known_folder = InternetCache | LocalAppData | Profile | Documents

external get_known_folder_path : known_folder -> string option
  = "geneweb__get_known_folder_path"

include Xdg

let windows_path ~getenv name known_folder =
  match getenv name with
  | Some s -> s
  | None -> (
      match get_known_folder_path known_folder with
      | Some s -> s
      | None -> Filename.current_dir_name)

let getenv_windows ~getenv s =
  match s with
  | "HOME" -> Some (windows_path ~getenv s Profile)
  | "XDG_DATA_HOME" -> Some (windows_path ~getenv s Documents)
  | "XDG_CONFIG_HOME" -> Some (windows_path ~getenv s LocalAppData)
  | "XDG_STATE_HOME" -> Some (windows_path ~getenv s LocalAppData)
  | "XDG_CACHE_HOME" -> Some (windows_path ~getenv s InternetCache)
  | "XDG_DATA_DIRS" -> Some (windows_path ~getenv s Documents)
  | "XDG_CONFIG_DIRS" -> Some (windows_path ~getenv s LocalAppData)
  | _ -> None

let make ?(getenv = Sys.getenv_opt) () =
  let getenv = if Sys.unix then getenv else getenv_windows ~getenv in
  Xdg.make ~getenv ()

type one = Var.one
type many = Var.many
type 'a var = 'a Var.t

let name = Var.name
let path = Var.path
let paths = Var.paths
let concat = Var.concat
let ( // ) = concat

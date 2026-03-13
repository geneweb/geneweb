type t = string

let empty = ""
let is_empty t = t == empty
let of_string s = s
let to_string t = t
let concat = Filename.concat

let of_segments = function
  | [] -> invalid_arg "Fpath.of_segments: empty list"
  | hd :: tl -> List.fold_left Filename.concat hd tl

let dirname = Filename.dirname
let basename = Filename.basename
let chop_extension = Filename.chop_extension
let remove_extension = Filename.remove_extension
let replace_extension t ext = Filename.remove_extension t ^ ext
let check_suffix = Filename.check_suffix
let extension t = Filename.extension t
let equal = String.equal
let compare = String.compare
let pp fmt t = Format.pp_print_string fmt t
let ( ~$ ) = of_string
let ( // ) = concat

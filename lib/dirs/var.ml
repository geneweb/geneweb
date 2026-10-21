type one = private One
type many = private Many

type _ content =
  | One : string -> one content
  | Many : string list -> many content

type 'a t = { name : string; content : 'a content }

let ( // ) = Filename.concat

let name ?(escaped = false) { name; _ } =
  if escaped then Fmt.str "\\$%s" name else Fmt.str "$%s" name

let concat { name; content = One s } suffix =
  let name = name ^ "/" ^ suffix in
  let content = One (s // suffix) in
  { name; content }

let[@inline] path { content = One v; _ } = v
let[@inline] paths { content = Many l; _ } = l

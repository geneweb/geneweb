module Y = Yojson.Safe
module U = Yojson.Safe.Util

let ( let* ) = Option.bind

type json = Yojson.Safe.t

(* According to the specification, a valid JSON-RPC v2.0 must contain
   a field "jsonrpc" whose the value is exactly this string. *)
let version = "2.0"
let to_msg l = `Assoc (("jsonrpc", `String version) :: l)

let is_jsonrpc j =
  try
    match U.member "jsonrpc" j with
    | `String v -> String.equal v version
    | _ -> false
  with U.Type_error _ -> false

module Id = struct
  type t = [ `String of string | `Int of int ]

  let hash = function `String s -> Hashtbl.hash s | `Int i -> i

  let equal id1 id2 =
    match (id1, id2) with
    | `String s1, `String s2 -> String.equal s1 s2
    | `Int i1, `Int i2 -> Int.equal i1 i2
    | _ -> false

  let to_json t = (t :> json)

  let of_json = function
    | `String s -> Some (`String s)
    | `Int i -> Some (`Int i)
    | #json -> None

  let pp ppf t = Y.pretty_print ppf @@ to_json t
end

module Structured = struct
  type t = [ `Assoc of (string * json) list | `List of json list ]

  let to_json t = (t :> json)

  let of_json = function
    | `Assoc l -> Some (`Assoc l)
    | `List l -> Some (`List l)
    | #json -> None

  let pp ppf t = Y.pretty_print ppf @@ to_json t
end

module Notification = struct
  type t = { meth : string; params : Structured.t option }

  let[@inline always] make ?params meth = { meth; params }

  let to_json t =
    match t.params with
    | Some p ->
        to_msg [ ("method", `String t.meth); ("params", Structured.to_json p) ]
    | None -> to_msg [ ("method", `String t.meth) ]

  let of_json j =
    if not @@ is_jsonrpc j then None
    else
      try
        match (U.member "id" j, U.member "method" j, U.member "params" j) with
        | `Null, `String meth, `Null -> Some (make meth)
        | `Null, `String meth, p ->
            let* params = Structured.of_json p in
            Some (make ~params meth)
        | _ -> None
      with U.Type_error _ -> None

  let pp ppf t = Y.pretty_print ppf @@ to_json t
end

module Request = struct
  type t = { id : Id.t; meth : string; params : Structured.t option }

  let[@inline always] make ?params id meth = { id; meth; params }

  let to_json t =
    match t.params with
    | Some p ->
        to_msg
          [
            ("id", Id.to_json t.id);
            ("method", `String t.meth);
            ("params", Structured.to_json p);
          ]
    | None -> to_msg [ ("id", Id.to_json t.id); ("method", `String t.meth) ]

  let of_json j =
    if not @@ is_jsonrpc j then None
    else
      try
        let* id = U.member "id" j |> Id.of_json in
        match (U.member "method" j, U.member "params" j) with
        | `String meth, `Null -> Some (make id meth)
        | `String meth, p ->
            let* params = Structured.of_json p in
            Some (make ~params id meth)
        | _ -> None
      with U.Type_error _ -> None

  let pp ppf t = Y.pretty_print ppf @@ to_json t
end

module Response = struct
  module Error = struct
    type t = { code : int; message : string; data : json option }

    let[@inline always] error ?data code message = { code; message; data }

    let parse_error ?data () =
      error ?data (-32_700)
        "Invalid JSON was received by the server. An error occurred on the \
         server while parsing the JSON text."

    let invalid_request ?data () =
      error ?data (-32_600) "The JSON sent is not a valid Request object."

    let method_not_found ?data () =
      error ?data (-32601) "The method does not exist / is not available."

    let invalid_params ?data () =
      error ?data (-32603) "Invalid method parameter(s)."

    let internal_error ?data () = error ?data (-32603) "Internal JSON-RPC error"

    let server_error ?data ~code msg =
      if code < -32_099 || code > -32_000 then Fmt.invalid_arg "internal_error";
      error ?data code msg

    let to_json { code; message; data } =
      match data with
      | Some data ->
          `Assoc
            [
              ("code", `Int code); ("message", `String message); ("data", data);
            ]
      | None -> `Assoc [ ("code", `Int code); ("message", `String message) ]

    let of_json j =
      try
        match (U.member "code" j, U.member "message" j, U.member "data" j) with
        | `Int code, `String message, `Null ->
            Some { code; message; data = None }
        | `Int code, `String message, data ->
            Some { code; message; data = Some data }
        | _ -> None
      with U.Type_error _ -> None

    let pp ppf t = Y.pretty_print ppf @@ to_json t
  end

  type t = { id : Id.t option; result : (json, Error.t) Result.t }

  let[@inline always] ok ~id obj = { id = Some id; result = Ok obj }
  let[@inline always] error ?id err = { id; result = Error err }

  let to_json t =
    match (t.id, t.result) with
    | Some id, Ok obj -> to_msg [ ("id", Id.to_json id); ("result", obj) ]
    | Some id, Error e ->
        to_msg [ ("id", Id.to_json id); ("error", Error.to_json e) ]
    | None, Error e -> to_msg [ ("id", `Null); ("error", Error.to_json e) ]
    | None, Ok _ ->
        (* A success message has always an identifier. *)
        assert false

  let of_json j =
    if not @@ is_jsonrpc j then None
    else
      try
        let error_opt = U.member "error" j |> Error.of_json in
        match (U.member "id" j, U.member "result" j, error_opt) with
        | `Null, `Null, Some e -> Some (error e)
        | id, `Null, Some e ->
            let* id = Id.of_json id in
            Some (error ~id e)
        | id, obj, None when obj <> `Null ->
            let* id = Id.of_json id in
            Some (ok ~id obj)
        | _ -> None
      with U.Type_error _ -> None

  let pp ppf t = Y.pretty_print ppf @@ to_json t
end

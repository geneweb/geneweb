module Y = Yojson.Safe
module U = Yojson.Safe.Util

type error = [ `Json_error of string | `Invalid_type of string ]

let pp_error ppf = function
  | `Json_error e -> Fmt.string ppf e
  | `Invalid_type e -> Fmt.string ppf e

type 'a res = ('a, error) result

let ( let* ) = Result.bind

let rec map_bind (f : 'a -> 'b res) (l : 'a list) : 'b list res =
  match l with
  | [] -> Ok []
  | x :: xs ->
      let* y = f x in
      let* ys = map_bind f xs in
      Ok (y :: ys)

let exn_to_json_error f json =
  try Ok (f json) with Yojson.Json_error e -> Error (`Json_error e)

let to_list_res (json : Y.t) : Y.t list res = exn_to_json_error U.to_list json
let to_bool_res (json : Y.t) : bool res = exn_to_json_error U.to_bool json
let to_float_res (json : Y.t) : float res = exn_to_json_error U.to_float json
let to_int_res (json : Y.t) : int res = exn_to_json_error U.to_int json
let to_string_res (json : Y.t) : string res = exn_to_json_error U.to_string json
let invalid_type fmt = Fmt.kstr (fun e -> Error (`Invalid_type e)) fmt

type 'a generic = {
  to_json : 'a -> Y.t;
  of_json : Y.t -> 'a res;
  pp : unit Fmt.t;
}

type 'a t =
  | Unit : unit t
  | Bool : bool t
  | Float : float t
  | Int : int t
  | List : 'a t -> 'a list t
  | Array : 'a t -> 'a array t
  | String : string t
  | Option : 'a t -> 'a option t
  | Tuple2 : 'a t * 'b t -> ('a * 'b) t
  | Tuple3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | Tuple4 : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) t
  | Gen : 'a generic -> 'a t

let rec val_of_json : type a. a t -> Y.t -> a res =
 fun t j ->
  match t with
  | Unit -> (
      match j with
      | `Assoc [ ("Unit", `Null) ] -> Ok ()
      | _ ->
          invalid_type "expected unit value, got %a" (Y.pretty_print ~std:true)
            j)
  | Bool -> to_bool_res j
  | Float -> to_float_res j
  | Int -> to_int_res j
  | List e ->
      let g = val_of_json e in
      let* l = to_list_res j in
      map_bind g l
  | Array e ->
      let g = val_of_json e in
      let* l = to_list_res j in
      let* l = map_bind g l in
      Ok (Array.of_list l)
  | String -> to_string_res j
  | Option e -> (
      match j with
      | `Assoc [ ("None", `Null) ] -> Ok None
      | `Assoc [ ("Some", j) ] ->
          let* v = val_of_json e j in
          Ok (Some v)
      | _ ->
          invalid_type "expected an option value, got %a"
            (Y.pretty_print ~std:true) j)
  | Tuple2 (e1, e2) -> (
      match j with
      | `List [ j1; j2 ] ->
          let* v1 = val_of_json e1 j1 in
          let* v2 = val_of_json e2 j2 in
          Ok (v1, v2)
      | _ ->
          invalid_type "expected a tuple of size 2, got %a"
            (Y.pretty_print ~std:true) j)
  | Tuple3 (e1, e2, e3) -> (
      match j with
      | `List [ j1; j2; j3 ] ->
          let* v1 = val_of_json e1 j1 in
          let* v2 = val_of_json e2 j2 in
          let* v3 = val_of_json e3 j3 in
          Ok (v1, v2, v3)
      | _ ->
          invalid_type "expected a tuple of size 3, got %a"
            (Y.pretty_print ~std:true) j)
  | Tuple4 (e1, e2, e3, e4) -> (
      match j with
      | `List [ j1; j2; j3; j4 ] ->
          let* v1 = val_of_json e1 j1 in
          let* v2 = val_of_json e2 j2 in
          let* v3 = val_of_json e3 j3 in
          let* v4 = val_of_json e4 j4 in
          Ok (v1, v2, v3, v4)
      | _ ->
          invalid_type "expected a tuple of size 4, got %a"
            (Y.pretty_print ~std:true) j)
  | Gen g -> g.of_json j

let rec val_to_json : type a. a t -> a -> Y.t =
 fun t v ->
  match t with
  | Unit -> `Assoc [ ("unit", `Null) ]
  | Bool -> `Bool v
  | Float -> `Float v
  | Int -> `Int v
  | List e ->
      let to_json = val_to_json e in
      `List (List.map to_json v)
  | Array e ->
      let to_json = val_to_json e in
      let l = Array.to_seq v |> List.of_seq in
      `List (List.map to_json l)
  | String -> `String v
  | Option e -> (
      match v with
      | Some v -> `Assoc [ ("Some", val_to_json e v) ]
      | None -> `Assoc [ ("None", `Null) ])
  | Tuple2 (e1, e2) ->
      let v1, v2 = v in
      let j1 = val_to_json e1 v1 in
      let j2 = val_to_json e2 v2 in
      `List [ j1; j2 ]
  | Tuple3 (e1, e2, e3) ->
      let v1, v2, v3 = v in
      let j1 = val_to_json e1 v1 in
      let j2 = val_to_json e2 v2 in
      let j3 = val_to_json e3 v3 in
      `List [ j1; j2; j3 ]
  | Tuple4 (e1, e2, e3, e4) ->
      let v1, v2, v3, v4 = v in
      let j1 = val_to_json e1 v1 in
      let j2 = val_to_json e2 v2 in
      let j3 = val_to_json e3 v3 in
      let j4 = val_to_json e4 v4 in
      `List [ j1; j2; j3; j4 ]
  | Gen g -> g.to_json v

let rec pp : type a. a t Fmt.t =
 fun ppf e ->
  match e with
  | Unit -> Fmt.pf ppf "unit"
  | Bool -> Fmt.pf ppf "bool"
  | Float -> Fmt.pf ppf "float"
  | Int -> Fmt.pf ppf "int"
  | List e -> Fmt.pf ppf "%a list" pp e
  | Array e -> Fmt.pf ppf "%a array" pp e
  | String -> Fmt.pf ppf "string"
  | Option e -> Fmt.pf ppf "%a option" pp e
  | Tuple2 (e1, e2) -> Fmt.pf ppf "%a * %a" pp e1 pp e2
  | Tuple3 (e1, e2, e3) -> Fmt.pf ppf "%a * %a * %a" pp e1 pp e2 pp e3
  | Tuple4 (e1, e2, e3, e4) ->
      Fmt.pf ppf "%a * %a * %a * %a" pp e1 pp e2 pp e3 pp e4
  | Gen g -> g.pp ppf ()

let enum ~name l =
  let to_json v =
    match List.assoc v l with
    | exception Not_found -> failwith "incomplete enum witness"
    | s -> `String s
  in
  let of_json =
    let rv = List.map (fun (k, v) -> (v, k)) l in
    fun j ->
      match j with
      | `String s -> (
          try Ok (List.assoc s rv)
          with Not_found ->
            invalid_type "expected constructor of type %s, got %a" name
              (Y.pretty_print ~std:true) j)
      | _ ->
          invalid_type "expected a string value, got %a"
            (Y.pretty_print ~std:true) j
  in
  let pp ppf () = Fmt.string ppf name in
  Gen { to_json; of_json; pp }

let generic ~to_json ~of_json ~pp = Gen { to_json; of_json; pp }

module Syntax = struct
  let unit = Unit
  let bool = Bool
  let float = Float
  let int = Int
  let option e = Option e
  let list e = List e
  let array e = Array e
  let string = String
  let tup2 e1 e2 = Tuple2 (e1, e2)
  let tup3 e1 e2 e3 = Tuple3 (e1, e2, e3)
  let tup4 e1 e2 e3 e4 = Tuple4 (e1, e2, e3, e4)
end

module Y = Yojson
module U = Yojson.Safe.Util

let ( let* ) = Option.bind

let rec map_bind (f : 'a -> 'b option) (l : 'a list) : 'b list option =
  match l with
  | [] -> Some []
  | x :: xs ->
      let* y = f x in
      let* ys = map_bind f xs in
      Some (y :: ys)

let to_list_option (json : Y.Safe.t) : Y.Safe.t list option =
  try Some (U.to_list json) with Y.Json_error _ -> None

type 'a generic = {
  to_json : 'a -> Y.Safe.t;
  of_json : Y.Safe.t -> 'a option;
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

let rec val_of_json : type a. a t -> Y.Safe.t -> a option =
 fun t j ->
  match t with
  | Unit -> ( match j with `Assoc [ ("Unit", `Null) ] -> Some () | _ -> None)
  | Bool -> U.to_bool_option j
  | Float -> U.to_float_option j
  | Int -> U.to_int_option j
  | List e ->
      let g = val_of_json e in
      let* l = to_list_option j in
      map_bind g l
  | Array e ->
      let g = val_of_json e in
      let* l = to_list_option j in
      let* l = map_bind g l in
      Some (Array.of_list l)
  | String -> U.to_string_option j
  | Option e -> (
      match j with
      | `Assoc [ ("None", `Null) ] -> Some None
      | `Assoc [ ("Some", j) ] -> Some (val_of_json e j)
      | _ -> None)
  | Tuple2 (e1, e2) -> (
      match j with
      | `List [ j1; j2 ] ->
          let* v1 = val_of_json e1 j1 in
          let* v2 = val_of_json e2 j2 in
          Some (v1, v2)
      | _ -> None)
  | Tuple3 (e1, e2, e3) -> (
      match j with
      | `List [ j1; j2; j3 ] ->
          let* v1 = val_of_json e1 j1 in
          let* v2 = val_of_json e2 j2 in
          let* v3 = val_of_json e3 j3 in
          Some (v1, v2, v3)
      | _ -> None)
  | Tuple4 (e1, e2, e3, e4) -> (
      match j with
      | `List [ j1; j2; j3; j4 ] ->
          let* v1 = val_of_json e1 j1 in
          let* v2 = val_of_json e2 j2 in
          let* v3 = val_of_json e3 j3 in
          let* v4 = val_of_json e4 j4 in
          Some (v1, v2, v3, v4)
      | _ -> None)
  | Gen g -> g.of_json j

let rec val_to_json : type a. a t -> a -> Y.Safe.t =
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
          match List.assoc s rv with exception Not_found -> None | v -> Some v)
      | _ -> None
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

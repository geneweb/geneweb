module Driver = Geneweb_db.Driver

type 'a t = (string, string * 'a) Hashtbl.t

let create () = Hashtbl.create 1

let get t ?(key = "") base compute =
  match Driver.dict_version base with
  | None -> compute ()
  | Some (bdir, version) -> (
      let k = bdir ^ "\000" ^ key in
      match Hashtbl.find_opt t k with
      | Some (v, x) when String.equal v version -> x
      | Some _ | None ->
          let x = compute () in
          Hashtbl.replace t k (version, x);
          x)

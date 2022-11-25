open Geneweb

let assets = ref ""
let registered = ref []

let ht : (string, string * (Config.config -> string option -> bool)) Hashtbl.t =
  Hashtbl.create 0

let register ~ns list =
  assert (not @@ List.mem ns !registered);
  registered := ns :: !registered;
  List.iter
    (fun (m, fn) ->
      let fn = fn !assets in
      Hashtbl.add ht m (ns, fn))
    list

let se : (string * (Config.config -> string option -> unit)) list ref = ref []
let register_se ~ns fn = Mutil.list_ref_append se (ns, fn !assets)

open Geneweb

let registered = ref []

let ht
  : (string, (string * (Config.config -> Gwdb.base -> bool))) Hashtbl.t
  = Hashtbl.create 0

let assets = ref ""

let register ~ns list =
  assert (not @@ List.mem ns !registered) ;
  registered := ns :: !registered ;
  List.iter (fun (m, fn) -> let fn = fn !assets in Hashtbl.add ht m (ns, fn)) list

let get = Hashtbl.find_all ht

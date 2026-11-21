let file () =
  Filename.concat (Filename.concat (Secure.base_dir ()) "etc") "ip_country.csv"

let unknown = "ZZ"

(* Key of an address, ordered like the lines of the file: a family tag, IPv4
   before IPv6, then the address in network order. An IPv4-mapped IPv6 address
   is looked up as IPv4. *)
let key a =
  match Ipaddr.to_v4 a with
  | Some v4 -> "4" ^ Ipaddr.V4.to_octets v4
  | None -> (
      match a with
      | Ipaddr.V6 v6 -> "6" ^ Ipaddr.V6.to_octets v6
      | Ipaddr.V4 v4 -> "4" ^ Ipaddr.V4.to_octets v4)

let unquote s =
  let s = String.trim s in
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2) else s

let parse_line l =
  match List.map unquote (String.split_on_char ',' l) with
  | first :: last :: cc :: _ when String.length cc = 2 -> (
      match (Ipaddr.of_string first, Ipaddr.of_string last) with
      | Ok first, Ok last ->
          Some (key first, key last, String.uppercase_ascii cc)
      | Error _, _ | _, Error _ -> None)
  | _ -> None

(* The first line starting at or after the offset [p], with its offset. *)
let line_from ic p =
  let skipped =
    if p = 0 then (
      seek_in ic 0;
      true)
    else (
      seek_in ic (p - 1);
      match input_line ic with _ -> true | exception End_of_file -> false)
  in
  if not skipped then None
  else
    let off = pos_in ic in
    match input_line ic with
    | l -> Some (off, l)
    | exception End_of_file -> None

(* Last range of the file whose first address is not after [k]: [lo, hi) are
   byte offsets, [best] the best range found before [lo]. A line that is not a
   range (a header) is passed over. *)
let rec search ic k lo hi best =
  if lo >= hi then best
  else
    let mid = lo + ((hi - lo) / 2) in
    match line_from ic mid with
    | Some (off, l) when off < hi -> (
        match parse_line l with
        | Some (first, _, _) as r when String.compare first k <= 0 ->
            search ic k (off + 1) hi r
        | Some _ -> search ic k lo mid best
        | None -> search ic k (off + 1) hi best)
    | Some _ | None -> search ic k lo mid best

let country ic addr =
  match Ipaddr.of_string (String.trim addr) with
  | Error _ -> unknown
  | Ok a -> (
      let k = key a in
      match search ic k 0 (in_channel_length ic) None with
      | Some (_, last, cc) when String.compare k last <= 0 -> cc
      | Some _ | None -> unknown)

let countries fname addrs =
  match Secure.open_in_bin fname with
  | exception Sys_error _ -> None
  | ic ->
      Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () ->
      let tbl = Hashtbl.create 64 in
      List.iter
        (fun a ->
          let cc = country ic a in
          let n = Option.value ~default:0 (Hashtbl.find_opt tbl cc) in
          Hashtbl.replace tbl cc (n + 1))
        addrs;
      Hashtbl.fold (fun cc n l -> (cc, n) :: l) tbl []
      |> List.sort (fun (c1, n1) (c2, n2) ->
          match Int.compare n2 n1 with 0 -> String.compare c1 c2 | c -> c)
      |> Option.some

let pp_sockaddr ppf s =
  match s with
  | Unix.ADDR_UNIX _ ->
      (* Cannot happen as these addresses are discarded in [try_addresses]. *)
      assert false
  | ADDR_INET (a, p) ->
      let addr = Unix.string_of_inet_addr a in
      if Unix.is_inet6_addr a then Fmt.pf ppf "[%s]:%d" addr p
      else Fmt.pf ppf "%s:%d" addr p

let is_lan_candidate = function
  | Unix.ADDR_INET (a, _) when not (Unix.is_inet6_addr a) ->
      a <> Unix.inet_addr_any
      && not (String.starts_with ~prefix:"127." (Unix.string_of_inet_addr a))
  | _ -> false

open Alcotest
open Geneweb_http

let inet s p = Unix.ADDR_INET (Unix.inet_addr_of_string s, p)

let pp_sockaddr_ipv4 () =
  (check string) "" "127.0.0.1:2316"
    (Fmt.str "%a" Server.pp_sockaddr (inet "127.0.0.1" 2316))

let pp_sockaddr_ipv6 () =
  (check string) "" "[::1]:2317"
    (Fmt.str "%a" Server.pp_sockaddr (inet "::1" 2317))

let pp_sockaddr_ipv6_wildcard () =
  (check string) "" "[::]:2317"
    (Fmt.str "%a" Server.pp_sockaddr (inet "::" 2317))

let lan_candidate_rejects_loopback () =
  (check bool) "" false (Server.is_lan_candidate (inet "127.0.0.1" 0));
  (check bool) "" false (Server.is_lan_candidate (inet "127.0.1.1" 0))

let lan_candidate_accepts_private_ipv4 () =
  (check bool) "" true (Server.is_lan_candidate (inet "192.168.2.4" 0))

let lan_candidate_rejects_ipv6 () =
  (check bool) "" false (Server.is_lan_candidate (inet "::1" 0));
  (check bool) "" false (Server.is_lan_candidate (inet "fe80::1" 0))

let lan_candidate_rejects_unix () =
  (check bool) "" false (Server.is_lan_candidate (Unix.ADDR_UNIX "/tmp/s"))

let lan_candidate_rejects_v4_mapped () =
  (check bool) "" false (Server.is_lan_candidate (inet "::ffff:192.168.2.4" 0))

let lan_candidate_accepts_public_ipv4 () =
  (check bool) "" true (Server.is_lan_candidate (inet "203.0.113.7" 0))

let lan_candidate_rejects_wildcard () =
  (check bool) "" false (Server.is_lan_candidate (inet "0.0.0.0" 0))

let v =
  [
    ( "server-pp_sockaddr",
      [
        test_case "ipv4" `Quick pp_sockaddr_ipv4;
        test_case "ipv6" `Quick pp_sockaddr_ipv6;
        test_case "ipv6 wildcard" `Quick pp_sockaddr_ipv6_wildcard;
      ] );
    ( "server-is_lan_candidate",
      [
        test_case "rejects loopback" `Quick lan_candidate_rejects_loopback;
        test_case "accepts private ipv4" `Quick
          lan_candidate_accepts_private_ipv4;
        test_case "rejects ipv6" `Quick lan_candidate_rejects_ipv6;
        test_case "rejects unix socket" `Quick lan_candidate_rejects_unix;
        test_case "rejects v4-mapped" `Quick lan_candidate_rejects_v4_mapped;
        test_case "accepts public ipv4" `Quick lan_candidate_accepts_public_ipv4;
        test_case "rejects wildcard" `Quick lan_candidate_rejects_wildcard;
      ] );
  ]

let () = Alcotest.run ~and_exit:false "Server" v

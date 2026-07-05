open Alcotest
open Geneweb_oidc

let claims_of s =
  match Oidc.claims_of_json_string s with
  | Ok c -> c
  | Error _ -> failwith ("invalid test JSON: " ^ s)

let sample =
  claims_of
    {|{
      "email": "alice@test.local",
      "name": "Alice Test",
      "sub": "uuid-123",
      "age": 42,
      "realm_access": { "roles": ["geneweb-wizard", "offline_access"] },
      "groups": ["geneweb-friend"],
      "single_role": "geneweb-wizard",
      "geneweb_person_key": "alice.0+de test"
    }|}

let claim_string_scalar () =
  let c = sample in
  (check (option string))
    "top-level string" (Some "alice@test.local")
    (Oidc.claim_string c "email");
  (check (option string))
    "name" (Some "Alice Test")
    (Oidc.claim_string c "name");
  (check (option string))
    "int converted to string" (Some "42")
    (Oidc.claim_string c "age");
  (check (option string))
    "missing claim" None
    (Oidc.claim_string c "does_not_exist");
  (check (option string))
    "array is not scalar" None
    (Oidc.claim_string c "groups")

let claim_string_dotted () =
  let c = sample in
  (check (option string))
    "nested array not scalar" None
    (Oidc.claim_string c "realm_access.roles");
  (check (option string))
    "missing nested path" None
    (Oidc.claim_string c "realm_access.missing");
  (check (option string))
    "deep missing path" None
    (Oidc.claim_string c "nope.roles");
  (check (option string))
    "person key scalar" (Some "alice.0+de test")
    (Oidc.claim_string c "geneweb_person_key")

let claim_has_value_array () =
  let c = sample in
  (check bool) "nested role present" true
    (Oidc.claim_has_value c ~path:"realm_access.roles" ~value:"geneweb-wizard");
  (check bool) "nested role absent" false
    (Oidc.claim_has_value c ~path:"realm_access.roles" ~value:"geneweb-friend");
  (check bool) "top-level group present" true
    (Oidc.claim_has_value c ~path:"groups" ~value:"geneweb-friend");
  (check bool) "top-level group absent" false
    (Oidc.claim_has_value c ~path:"groups" ~value:"geneweb-wizard")

let claim_has_value_scalar () =
  let c = sample in
  (check bool) "scalar role match" true
    (Oidc.claim_has_value c ~path:"single_role" ~value:"geneweb-wizard");
  (check bool) "scalar role mismatch" false
    (Oidc.claim_has_value c ~path:"single_role" ~value:"geneweb-friend");
  (check bool) "missing path is false" false
    (Oidc.claim_has_value c ~path:"missing.path" ~value:"x")

let is_ok = function Ok () -> true | Error _ -> false

let mk_claims ~iss ~aud ~nonce =
  claims_of
    (Printf.sprintf {|{ "iss": %s, "aud": %s, "exp": 9999999999, "nonce": %s }|}
       iss aud nonce)

let validate_claims_ok () =
  let c = mk_claims ~iss:{|"https://idp"|} ~aud:{|"client1"|} ~nonce:{|"n1"|} in
  (check bool) "all valid" true
    (is_ok
       (Oidc.validate_claims ~client_id:"client1" ~issuer:"https://idp"
          ~nonce:"n1" c));
  let c_arr =
    mk_claims ~iss:{|"https://idp"|} ~aud:{|["other", "client1"]|}
      ~nonce:{|"n1"|}
  in
  (check bool) "aud array contains client_id" true
    (is_ok
       (Oidc.validate_claims ~client_id:"client1" ~issuer:"https://idp"
          ~nonce:"n1" c_arr))

let validate_claims_rejects () =
  let base =
    mk_claims ~iss:{|"https://idp"|} ~aud:{|"client1"|} ~nonce:{|"n1"|}
  in
  (check bool) "iss mismatch" false
    (is_ok
       (Oidc.validate_claims ~client_id:"client1" ~issuer:"https://evil"
          ~nonce:"n1" base));
  (check bool) "aud mismatch" false
    (is_ok
       (Oidc.validate_claims ~client_id:"other" ~issuer:"https://idp"
          ~nonce:"n1" base));
  (check bool) "nonce mismatch" false
    (is_ok
       (Oidc.validate_claims ~client_id:"client1" ~issuer:"https://idp"
          ~nonce:"n2" base));
  let expired =
    claims_of
      {|{ "iss": "https://idp", "aud": "client1", "exp": 1000000000, "nonce": "n1" }|}
  in
  (check bool) "expired exp" false
    (is_ok
       (Oidc.validate_claims ~client_id:"client1" ~issuer:"https://idp"
          ~nonce:"n1" expired));
  let no_nonce =
    claims_of {|{ "iss": "https://idp", "aud": "client1", "exp": 9999999999 }|}
  in
  (check bool) "missing nonce" false
    (is_ok
       (Oidc.validate_claims ~client_id:"client1" ~issuer:"https://idp"
          ~nonce:"n1" no_nonce))

let base64url_decode_cases () =
  let dec s =
    match Oidc.base64url_decode s with
    | Ok d -> d
    | Error _ -> failwith "decode"
  in
  (check string) "hello" "hello" (dec "aGVsbG8");
  (check string) "url-safe bytes" "\250\251" (dec "-vs");
  (check bool) "invalid input errors" true
    (match Oidc.base64url_decode "!!!" with Error _ -> true | Ok _ -> false)

let pkce_cases () =
  (* RFC 7636 Appendix B known-answer vector for the S256 method. *)
  (check string) "S256 challenge" "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
    (Oidc.code_challenge "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk");
  let verifier = Oidc.generate_code_verifier () in
  (check int) "verifier length" 43 (String.length verifier);
  (check bool) "verifier is url-safe unreserved" true
    (String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' -> true
         | _ -> false)
       verifier)

let v =
  [
    ( "oidc-claim-string",
      [
        test_case "claim_string scalar" `Quick claim_string_scalar;
        test_case "claim_string dotted path" `Quick claim_string_dotted;
      ] );
    ( "oidc-claim-has-value",
      [
        test_case "claim_has_value array" `Quick claim_has_value_array;
        test_case "claim_has_value scalar" `Quick claim_has_value_scalar;
      ] );
    ( "oidc-validate-claims",
      [
        test_case "accepts valid claims" `Quick validate_claims_ok;
        test_case "rejects invalid claims" `Quick validate_claims_rejects;
      ] );
    ( "oidc-base64url",
      [ test_case "base64url_decode" `Quick base64url_decode_cases ] );
    ("oidc-pkce", [ test_case "code_challenge S256" `Quick pkce_cases ]);
  ]

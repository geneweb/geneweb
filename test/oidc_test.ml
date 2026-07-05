open Alcotest
open Geneweb_oidc

let claims_of s =
  match Oidc.claims_of_json_string s with
  | Ok c -> c
  | Error _ -> failwith ("invalid test JSON: " ^ s)

(* Sample token resembling a Keycloak id_token: nested realm_access.roles
   array, a top-level groups array (Authentik style), and a custom
   geneweb_person_key scalar claim. *)
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
      "geneweb_person_key": "christian.0+de vulpillieres"
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
  (* An array node is not a scalar *)
  (check (option string))
    "array is not scalar" None
    (Oidc.claim_string c "groups")

let claim_string_dotted () =
  let c = sample in
  (* realm_access.roles is an array -> not a scalar *)
  (check (option string))
    "nested array not scalar" None
    (Oidc.claim_string c "realm_access.roles");
  (* navigating through a missing intermediate node *)
  (check (option string))
    "missing nested path" None
    (Oidc.claim_string c "realm_access.missing");
  (check (option string))
    "deep missing path" None
    (Oidc.claim_string c "nope.roles");
  (check (option string))
    "person key scalar" (Some "christian.0+de vulpillieres")
    (Oidc.claim_string c "geneweb_person_key")

let claim_has_value_array () =
  let c = sample in
  (* nested array membership (Keycloak realm_access.roles) *)
  (check bool) "nested role present" true
    (Oidc.claim_has_value c ~path:"realm_access.roles" ~value:"geneweb-wizard");
  (check bool) "nested role absent" false
    (Oidc.claim_has_value c ~path:"realm_access.roles" ~value:"geneweb-friend");
  (* top-level array membership (Authentik groups) *)
  (check bool) "top-level group present" true
    (Oidc.claim_has_value c ~path:"groups" ~value:"geneweb-friend");
  (check bool) "top-level group absent" false
    (Oidc.claim_has_value c ~path:"groups" ~value:"geneweb-wizard")

let claim_has_value_scalar () =
  let c = sample in
  (* single string claim equal to value *)
  (check bool) "scalar role match" true
    (Oidc.claim_has_value c ~path:"single_role" ~value:"geneweb-wizard");
  (check bool) "scalar role mismatch" false
    (Oidc.claim_has_value c ~path:"single_role" ~value:"geneweb-friend");
  (* missing path *)
  (check bool) "missing path is false" false
    (Oidc.claim_has_value c ~path:"missing.path" ~value:"x")

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
  ]

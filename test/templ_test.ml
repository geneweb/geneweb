open Alcotest
module Templ = Geneweb.Templ

let mask ~wizard key value = Templ.masked_bvar_value ~wizard key value

let oidc_secret_masked () =
  check string "wizard sees mask" "####"
    (mask ~wizard:true "oidc_client_secret" "s3cr3t");
  check string "non-wizard sees mask" "####"
    (mask ~wizard:false "oidc_client_secret" "s3cr3t")

let oidc_secret_file_masked () =
  check string "wizard sees value" "/path/to/secret"
    (mask ~wizard:true "oidc_client_secret_file" "/path/to/secret");
  check string "non-wizard sees mask" "####"
    (mask ~wizard:false "oidc_client_secret_file" "/path/to/secret")

let other_bvars_unchanged () =
  check string "wizard sees wizard_passwd" "pw"
    (mask ~wizard:true "wizard_passwd" "pw");
  check string "non-wizard sees mask for wizard_passwd" "####"
    (mask ~wizard:false "wizard_passwd" "pw");
  check string "empty masked value stays empty" ""
    (mask ~wizard:false "friend_passwd" "");
  check string "plain parameter always visible" "yes"
    (mask ~wizard:false "use_restrict" "yes")

let v =
  [
    ( "bvar masking",
      [
        test_case "OIDC client secret is never displayed" `Quick
          oidc_secret_masked;
        test_case "OIDC secret file is hidden from non-wizards" `Quick
          oidc_secret_file_masked;
        test_case "Existing masking behaviour unchanged" `Quick
          other_bvars_unchanged;
      ] );
  ]

(** OIDC (OpenID Connect) client for GeneWeb.

    Implements the Authorization Code Flow with PKCE per OpenID Connect Core
    1.0. Uses curl for HTTP requests. The id_token is obtained over the TLS
    back-channel to the token endpoint, so its JWS signature is not checked
    locally (OIDC Core 3.1.3.7). *)

(** {1 Types} *)

type provider_config = {
  issuer : string;
  authorization_endpoint : string;
  token_endpoint : string;
  end_session_endpoint : string option;
}
(** OIDC provider configuration, obtained via discovery. *)

type claims
(** Decoded JWT claims (opaque JSON object). Access values with {!claim_string}
    and {!claim_has_value}, which support dotted paths and array claims. *)

type token_response = { id_token : string; access_token : string option }
(** Token endpoint response. *)

(** {1 Errors} *)

type error = Http_error of string | Json_error of string | Jwt_error of string

val pp_error : Format.formatter -> error -> unit

(** {1 Discovery} *)

val discover : string -> (provider_config, error) result
(** [discover issuer_url] fetches the OpenID Connect discovery document from
    [issuer_url/.well-known/openid-configuration]. *)

(** {1 Authorization} *)

val authorization_url :
  provider_config ->
  client_id:string ->
  redirect_uri:string ->
  state:string ->
  nonce:string ->
  code_challenge:string ->
  string
(** [authorization_url provider ~client_id ~redirect_uri ~state ~nonce
     ~code_challenge] builds the authorization endpoint URL with the required
    OIDC parameters (response_type=code, scope=openid email profile) and the
    PKCE parameters (code_challenge, code_challenge_method=S256). *)

(** {1 Token Exchange} *)

val exchange_code :
  provider_config ->
  client_id:string ->
  client_secret:string ->
  redirect_uri:string ->
  code:string ->
  code_verifier:string ->
  (token_response, error) result
(** [exchange_code provider ~client_id ~client_secret ~redirect_uri ~code
     ~code_verifier] exchanges an authorization code for tokens via HTTP POST to
    the token endpoint, sending the PKCE [code_verifier]. Uses
    client_secret_post authentication method. Returns the token response
    containing the id_token. *)

(** {1 ID Token} *)

val decode_and_validate_id_token :
  client_id:string ->
  issuer:string ->
  nonce:string ->
  string ->
  (claims, error) result
(** [decode_and_validate_id_token ~client_id ~issuer ~nonce token] decodes a JWT
    id_token's payload and validates the standard claims (iss, aud, exp, nonce).
    It does {b not} verify the JWS signature: the token is fetched directly from
    the token endpoint over TLS, which authenticates the issuer (OIDC Core
    3.1.3.7). Returns the decoded claims on success. *)

val validate_claims :
  client_id:string ->
  issuer:string ->
  nonce:string ->
  claims ->
  (unit, error) result
(** [validate_claims ~client_id ~issuer ~nonce claims] checks the standard OIDC
    id_token claims: [iss] equals [issuer], [aud] contains [client_id], [exp] is
    in the future (with 60s leeway), and [nonce] matches. Applied automatically
    by {!decode_and_validate_id_token}; exposed for testing. *)

val base64url_decode : string -> (string, error) result
(** [base64url_decode s] decodes an unpadded base64url string (RFC 7515).
    Exposed for testing. *)

(** {1 Claim Access} *)

val claims_of_json_string : string -> (claims, error) result
(** [claims_of_json_string s] parses a JSON object string into claims. Returns
    an error if [s] is not valid JSON or not an object. Primarily useful for
    testing the claim accessors. *)

val claim_string : claims -> string -> string option
(** [claim_string claims path] returns the scalar string value at [path] in the
    claims, or [None] if absent or not a scalar. [path] is a dotted path
    navigating nested JSON objects, e.g. ["email"] or ["realm_access.roles"].
    Integers and booleans are converted to strings. *)

val claim_has_value : claims -> path:string -> value:string -> bool
(** [claim_has_value claims ~path ~value] is [true] if the node at [path] equals
    [value] (when it is a scalar) or is an array containing [value]. Used for
    role/group membership tests, e.g.
    [claim_has_value claims ~path:"realm_access.roles" ~value:"geneweb-wizard"].
*)

(** {1 Logout} *)

val logout_url :
  provider_config ->
  id_token_hint:string ->
  post_logout_redirect_uri:string ->
  string option
(** [logout_url provider ~id_token_hint ~post_logout_redirect_uri] builds the
    RP-Initiated Logout URL if the provider supports it. Returns [None] if the
    provider has no end_session_endpoint. *)

(** {1 State Management} *)

val generate_state : unit -> string
(** [generate_state ()] returns a cryptographically random state string. *)

val generate_nonce : unit -> string
(** [generate_nonce ()] returns a cryptographically random nonce string. *)

val generate_token : unit -> string
(** [generate_token ()] returns a cryptographically random 256-bit token,
    hex-encoded. Suitable for use as an opaque session identifier. *)

val generate_code_verifier : unit -> string
(** [generate_code_verifier ()] returns a fresh PKCE code verifier (RFC 7636): a
    256-bit CSPRNG value, base64url-encoded without padding. *)

val code_challenge : string -> string
(** [code_challenge verifier] is the S256 PKCE challenge for [verifier], i.e.
    the base64url-encoded (no padding) SHA-256 of [verifier]. *)

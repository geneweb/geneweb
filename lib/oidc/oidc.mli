(** OIDC (OpenID Connect) client for GeneWeb.

    Implements the Authorization Code Flow per OpenID Connect Core 1.0. Uses
    curl for HTTP requests and mirage-crypto-pk for JWT verification. *)

(** {1 Types} *)

type provider_config = {
  issuer : string;
  authorization_endpoint : string;
  token_endpoint : string;
  jwks_uri : string;
  end_session_endpoint : string option;
}
(** OIDC provider configuration, obtained via discovery. *)

type claims
(** Decoded JWT claims (opaque JSON object). Access values with {!claim_string}
    and {!claim_has_value}, which support dotted paths and array claims. *)

type jwk = {
  kid : string;
  n : string;  (** base64url-encoded RSA modulus *)
  e : string;  (** base64url-encoded RSA exponent *)
}
(** An RSA JSON Web Key. *)

type token_response = { id_token : string; access_token : string option }
(** Token endpoint response. *)

(** {1 Errors} *)

type error =
  | Http_error of string
  | Json_error of string
  | Jwt_error of string
  | Config_error of string

val pp_error : Format.formatter -> error -> unit

(** {1 Discovery} *)

val discover : string -> (provider_config, error) result
(** [discover issuer_url] fetches the OpenID Connect discovery document from
    [issuer_url/.well-known/openid-configuration]. Results are cached in-process
    after the first successful call. *)

(** {1 Authorization} *)

val authorization_url :
  provider_config ->
  client_id:string ->
  redirect_uri:string ->
  state:string ->
  nonce:string ->
  string
(** [authorization_url provider ~client_id ~redirect_uri ~state ~nonce] builds
    the authorization endpoint URL with required OIDC parameters:
    response_type=code, scope=openid email profile. *)

(** {1 Token Exchange} *)

val exchange_code :
  provider_config ->
  client_id:string ->
  client_secret:string ->
  redirect_uri:string ->
  code:string ->
  (token_response, error) result
(** [exchange_code provider ~client_id ~client_secret ~redirect_uri ~code]
    exchanges an authorization code for tokens via HTTP POST to the token
    endpoint. Uses client_secret_post authentication method. Returns the token
    response containing the id_token. *)

(** {1 JWT Verification} *)

val fetch_jwks : string -> (jwk list, error) result
(** [fetch_jwks jwks_uri] fetches and parses the JWKS from the provider. Results
    are cached; cache is invalidated when a JWT contains an unknown kid. *)

val verify_and_decode_id_token :
  jwks_uri:string ->
  client_id:string ->
  issuer:string ->
  nonce:string ->
  string ->
  (claims, error) result
(** [verify_and_decode_id_token ~jwks_uri ~client_id ~issuer ~nonce token]
    decodes a JWT id_token, verifies the RS256 signature against the provider's
    JWKS, and validates the standard claims (iss, aud, exp, nonce). Returns the
    decoded claims on success. *)

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

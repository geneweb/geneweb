(** OIDC (OpenID Connect) Authorization Code Flow client with PKCE, using curl
    for HTTP. The id_token comes over TLS from the token endpoint, so its
    signature is not checked locally (OIDC Core 3.1.3.7). *)

(** {1 Types} *)

type provider_config = {
  issuer : string;
  authorization_endpoint : string;
  token_endpoint : string;
  end_session_endpoint : string option;
}
(** OIDC provider configuration, obtained via discovery. *)

type claims
(** Decoded JWT claims. Read with {!claim_string} / {!claim_has_value}. *)

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
(** Build the authorization endpoint URL (response_type=code, scope "openid
    email profile", state, nonce, and the S256 PKCE challenge). *)

(** {1 Token Exchange} *)

val exchange_code :
  provider_config ->
  client_id:string ->
  client_secret:string ->
  redirect_uri:string ->
  code:string ->
  code_verifier:string ->
  (token_response, error) result
(** Exchange an authorization code for tokens at the token endpoint, with PKCE
    and client_secret_post authentication. *)

(** {1 ID Token} *)

val decode_and_validate_id_token :
  client_id:string ->
  issuer:string ->
  nonce:string ->
  string ->
  (claims, error) result
(** Decode an id_token payload and validate iss, aud, exp and nonce. The JWS
    signature is not checked (see the module note). *)

val validate_claims :
  client_id:string ->
  issuer:string ->
  nonce:string ->
  claims ->
  (unit, error) result
(** Validate iss, aud, exp (60s leeway) and nonce. Called by
    {!decode_and_validate_id_token}; exposed for testing. *)

val base64url_decode : string -> (string, error) result
(** Decode an unpadded base64url string (RFC 7515). Exposed for testing. *)

(** {1 Claim Access} *)

val claims_of_json_string : string -> (claims, error) result
(** Parse a JSON object string into claims. Exposed for testing. *)

val claim_string : claims -> string -> string option
(** Scalar value at dotted [path] (e.g. ["realm_access.roles"]), or [None]. Ints
    and bools are stringified. *)

val claim_has_value : claims -> path:string -> value:string -> bool
(** [true] if the node at dotted [path] equals [value], or is an array
    containing it. Used for role/group membership. *)

(** {1 Logout} *)

val logout_url :
  provider_config ->
  id_token_hint:string ->
  post_logout_redirect_uri:string ->
  string option
(** RP-initiated logout URL, or [None] if the provider has no
    end_session_endpoint. *)

(** {1 State Management} *)

val generate_state : unit -> string
(** [generate_state ()] returns a cryptographically random state string. *)

val generate_nonce : unit -> string
(** [generate_nonce ()] returns a cryptographically random nonce string. *)

val generate_token : unit -> string
(** Random 256-bit token, hex-encoded (opaque session identifier). *)

val generate_code_verifier : unit -> string
(** PKCE code verifier (RFC 7636): 256-bit CSPRNG, base64url, unpadded. *)

val code_challenge : string -> string
(** S256 PKCE challenge: base64url(SHA-256(verifier)), unpadded. *)

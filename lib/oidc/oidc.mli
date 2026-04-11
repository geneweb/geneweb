(** OIDC (OpenID Connect) client for GeneWeb.

    Implements the Authorization Code Flow per OpenID Connect Core 1.0.
    Uses curl for HTTP requests and mirage-crypto-pk for JWT verification. *)

(** {1 Types} *)

(** OIDC provider configuration, obtained via discovery. *)
type provider_config = {
  issuer : string;
  authorization_endpoint : string;
  token_endpoint : string;
  jwks_uri : string;
  end_session_endpoint : string option;
}

(** Decoded JWT claims as key-value pairs. *)
type claims = (string * string) list

(** An RSA JSON Web Key. *)
type jwk = {
  kid : string;
  n : string;  (** base64url-encoded RSA modulus *)
  e : string;  (** base64url-encoded RSA exponent *)
}

(** Token endpoint response. *)
type token_response = {
  id_token : string;
  access_token : string option;
}

(** OIDC user role mapping entry. *)
type user_mapping = {
  claim_value : string;
  role : [ `Wizard | `Friend ];
  display_name : string;
  person_key : string;
      (** GeneWeb person key (e.g. "christian.0 de vulpillieres").
          Empty string if not linked to a person in the database. *)
}

(** {1 Errors} *)

type error =
  | Http_error of string
  | Json_error of string
  | Jwt_error of string
  | Config_error of string

val pp_error : Format.formatter -> error -> unit

(** {1 Discovery} *)

(** [discover issuer_url] fetches the OpenID Connect discovery document
    from [issuer_url/.well-known/openid-configuration].
    Results are cached in-process after the first successful call. *)
val discover : string -> (provider_config, error) result

(** {1 Authorization} *)

(** [authorization_url provider ~client_id ~redirect_uri ~state ~nonce]
    builds the authorization endpoint URL with required OIDC parameters:
    response_type=code, scope=openid email profile. *)
val authorization_url :
  provider_config ->
  client_id:string ->
  redirect_uri:string ->
  state:string ->
  nonce:string ->
  string

(** {1 Token Exchange} *)

(** [exchange_code provider ~client_id ~client_secret ~redirect_uri ~code]
    exchanges an authorization code for tokens via HTTP POST to the token
    endpoint. Uses client_secret_post authentication method.
    Returns the token response containing the id_token. *)
val exchange_code :
  provider_config ->
  client_id:string ->
  client_secret:string ->
  redirect_uri:string ->
  code:string ->
  (token_response, error) result

(** {1 JWT Verification} *)

(** [fetch_jwks jwks_uri] fetches and parses the JWKS from the provider.
    Results are cached; cache is invalidated when a JWT contains an
    unknown kid. *)
val fetch_jwks : string -> (jwk list, error) result

(** [verify_and_decode_id_token ~jwks_uri ~client_id ~issuer ~nonce token]
    decodes a JWT id_token, verifies the RS256 signature against the
    provider's JWKS, and validates the standard claims (iss, aud, exp, nonce).
    Returns the decoded claims on success. *)
val verify_and_decode_id_token :
  jwks_uri:string ->
  client_id:string ->
  issuer:string ->
  nonce:string ->
  string ->
  (claims, error) result

(** {1 User Mapping} *)

(** [read_users_file filename] reads an OIDC user mapping file.
    Format: [claim_value:role:display_name] or
    [claim_value:role:display_name:person_key] per line.
    Lines starting with [#] are comments. *)
val read_users_file : string -> (user_mapping list, error) result

(** [lookup_user mappings claim_value] finds the role for a given
    claim value. Returns [None] for unmapped users (treated as visitors). *)
val lookup_user : user_mapping list -> string -> user_mapping option

(** {1 Logout} *)

(** [logout_url provider ~id_token_hint ~post_logout_redirect_uri]
    builds the RP-Initiated Logout URL if the provider supports it.
    Returns [None] if the provider has no end_session_endpoint. *)
val logout_url :
  provider_config ->
  id_token_hint:string ->
  post_logout_redirect_uri:string ->
  string option

(** {1 State Management} *)

(** [generate_state ()] returns a cryptographically random state string. *)
val generate_state : unit -> string

(** [generate_nonce ()] returns a cryptographically random nonce string. *)
val generate_nonce : unit -> string

module IO = Mirage_http_io

module type S = sig
  type +'a io

  (** The type for HTTP requests. *)
  type req

  (** The type for HTTP responses. *)
  type resp

  (** The type of the buffer. *)
  type raw = Cstruct.t

  type body = unit -> (raw * int * int) option Lwt.t

  type uri = Uri.t

  module HTTP : sig
    (** The type for header maps. A header map represents a list of HTTP
        headers. It maps header field names to their value. *)
    type headers

    (** The type for representing [HTTP-Version] fields. Both integers must be
        in the interval [\[0;9\]]. *)
    type version = int * int

    (** The type for HTTP request methods. *)
    type meth =
      [ `CONNECT
      | `DELETE
      | `GET
      | `HEAD
      | `OPTIONS
      | `Other of string
      | `POST
      | `PUT
      | `TRACE
      | `PATCH ]

    (** Headers. FIXME(dbuenzli): The semantic of this module considers than a
        field can contain multiple values. However, it's should be the wrong
        way to abstract values of headers - and let the user to consider than
        specific field can have multiple values. *)
    module Headers : sig
      (** The type for lower-cased HTTP header [field-name]s. *)
      type name

      val name : string -> name
      (** [name s] is a header name from [s]. @raise Invalid_argument if [s] is
          not a field-name. *)

      val name_equal : name -> name -> bool
      (** [name_equal n n'] is [true] iff [n] and [n'] are equal. *)

      (** {1:map Header maps} *)

      val empty : headers
      (** [empty] is the empty header map. *)

      val is_empty : headers -> bool
      (** [is_empty hs] is [true] iff [hs] is the empty header map. *)

      val find : name -> headers -> string option
      (** [find n hs] is the value of header [n] in [hs] (if defined). If [n]
          is multi-valued and defined, we return [String.concat "," vs] where
          [vs] are values related to [n] in [hs]. *)

      val get : name -> headers -> string
      (** [get n hs] is like {!find} but @raise Invalid_argument if [n] is
          undefined in [hs]. *)

      val pp : headers Fmt.t
      (** Pretty-printer of headers. *)

      (** {1:hcst Header name values} *)

      val user_agent : name
      (** User-Agent. *)

      val content_type : name
      (** Content-Type. *)

      val access_control_allow_origin : name
      (** Access-Control-Allow-Origin. *)

      val access_control_allow_methods : name
      (** Access-Control-Allow-Methods. *)

      val access_control_allow_headers : name
      (** Access-Control-Allow-Headers. *)

      val def : name -> string -> headers -> headers
      (** [def n v hs] is [hs] with [n] bound to [v]. *)

      val def_multi : name -> string list -> headers -> headers
      (** [def_multi n vs hs] is [hs] with [n] bound to the multi-value [vs].
          @raise Invalid_argument if [vs] is [[]]. *)

      val merge : headers -> headers -> headers
    end
  end

  (** {1:status_codes Status codes} *)

  (** The type for HTTP status code. *)
  type status = int

  val s100_continue : status
  (** [100]. *)

  val s200_ok : status
  (** [200]. *)

  module Request : sig
    (** The type for request bodies. *)

    val body : req -> body option

    val meth : req -> HTTP.meth

    val uri : req -> uri

    val headers : req -> HTTP.headers
    (** [headers r] is [r]'s information. Initially empty it can be used be
        services and layers to store and share data. *)

    val with_headers : req -> HTTP.headers -> req
    (** [with_headers req hs] is [req] with headers [hs]. *)

    val with_uri : req -> uri -> req
    (** [with_uri req p] is [req] with uri [p]. *)

    val with_body : req -> body -> req
    (** [with_body req body] is [req] with body [body]. *)

    val v :
         ?version:HTTP.version
      -> HTTP.meth
      -> path:uri
      -> ?body:body
      -> HTTP.headers
      -> req
    (** [v meth ~path headers body] is an HTTP request with the given
        components. [query] defaults to [None]. *)
  end

  module Response : sig
    val with_headers : resp -> HTTP.headers -> resp
    (** [with_headers resp hs] is [resp] with headers [hs]. *)

    val with_status : resp -> status -> resp
    (** [with_status resp p] is [resp] with status [p]. *)

    val with_body : resp -> body -> resp
    (** [with_body resp body] is [resp] with body [body]. *)

    val v :
      ?version:HTTP.version -> body:body -> HTTP.headers -> status -> resp

    val body : resp -> body
    (** [body r] is [r]'s body. *)

    val headers : resp -> HTTP.headers
    (** [headers r] is [r]'s headers. *)

    val status : resp -> status
    (** [status r] is [r]'s status. *)
  end

  module Client (CON : Conduit_mirage.S) : sig
    type t

    val request : t -> req -> resp io

    val connect : Resolver_lwt.t -> CON.t -> t io
  end

  module Server (CON : Conduit_mirage.S) : sig
    module IO : IO.S

    type connection_handler

    type t

    type ic = IO.ic

    type oc = IO.oc

    type error = unit

    type managed = ic -> oc -> unit io

    type reqd

    type response =
      [ `Response of resp
      | `Expert of resp * managed ]

    type error_handler = error -> unit io

    type request_handler = reqd -> response io

    val get_request : reqd -> req

    val create_connection_handler :
      request_handler -> error_handler -> connection_handler io

    val listen : t -> Conduit_mirage.server -> connection_handler -> unit io

    val connect : CON.t -> t io
  end
end

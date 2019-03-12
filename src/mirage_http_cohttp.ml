type +'a io = 'a Lwt.t

type raw = Cstruct.t

type uri = Uri.t

type body = unit -> (raw * int * int) option Lwt.t

type req =
  { req : Cohttp.Request.t
  ; uri : uri
  ; body : body option }

type resp =
  { resp : Cohttp.Response.t
  ; body : Cohttp_lwt.Body.t }

let stream_to_body body =
  Lwt_stream.from body
  |> Lwt_stream.map (fun (x, ofs, len) ->
         Cstruct.sub x ofs len |> Cstruct.to_string )
  |> Cohttp_lwt.Body.of_stream

let body_to_stream body =
  let stream =
    Cohttp_lwt.Body.to_stream body
    |> Lwt_stream.map (fun str -> (Cstruct.of_string str, 0, String.length str)
       )
  in
  fun () -> Lwt_stream.get stream

module HTTP = struct
  type headers = Cohttp.Header.t

  type meth = Cohttp.Code.meth

  type version = int * int

  module Headers = struct
    type name = string

    let name = String.lowercase_ascii

    let name_equal a b = String.equal a b

    let empty = Cohttp.Header.init ()

    let is_empty _ = assert false

    (* TODO *)
    let find name headers = Cohttp.Header.get headers name

    let def name value headers =
      let values = Astring.String.cuts ~empty:false ~sep:"," value in
      Cohttp.Header.add_multi headers name values

    let def_multi name values headers =
      if List.length values = 0 then
        raise (Invalid_argument "HTTP.Headers.def_multi: empty values");
      Cohttp.Header.add_multi headers name values

    let get x headers =
      match find x headers with
      | Some v ->
          v
      | None ->
          raise (Invalid_argument "HTTP.Headers.get: invalid name")

    let pp = Cohttp.Header.pp_hum

    let user_agent = "User-Agent"

    let content_type = "Content-Type"

    let access_control_allow_origin = "Access-Control-Allow-Origin"

    let access_control_allow_methods = "Access-Control-Allow-Methods"

    let access_control_allow_headers = "Access-Control-Allow-Headers"

    let of_list lst =
      List.map (fun (a,b) -> (name a, b)) lst
      |> Cohttp.Header.of_list


    let merge a b =
      Cohttp.Header.fold
        (fun key value acc -> Cohttp.Header.add_unless_exists acc key value)
        a b
  end
end

type status = int

let s100_continue = 100

let s200_ok = 200

module Request = struct
  let headers {req; _} = Cohttp.Request.headers req

  let with_headers x headers =
    {x with req = {x.req with Cohttp.Request.headers}}

  let with_body (x : req) body = {x with body = Some body}

  let with_uri (x : req) uri =
    let req = {x.req with resource = Uri.path uri} in
    {{x with req} with uri}


  let v ?(version = (1, 1)) meth ~path ?body headers =
    let req =
      { Cohttp.Request.headers
      ; meth
      ; resource = Uri.path path
      ; version =
          ( match version with
          | 1, 0 ->
              `HTTP_1_0
          | 1, 1 ->
              `HTTP_1_1
          | a, b ->
              raise
                (Invalid_argument
                   (Fmt.strf "Request.v: invalid version %d.%d" a b)) )
      ; encoding = Cohttp.Transfer.Chunked }
    in
    {req; uri=path; body}

  let body ({body; _} : req) = body

  let uri {req; _} = Cohttp.Request.uri req

  let meth {req; _} = Cohttp.Request.meth req
end

module Response = struct
  let with_headers x headers =
    {x with resp = {x.resp with Cohttp.Response.headers}}

  let with_status x status =
    let status = Cohttp.Code.status_of_code status in
    {x with resp = {x.resp with status}}

  let with_body (x : resp) body =
    let body = stream_to_body body in
    {x with body}

  let pp fmt res = Cohttp.Response.pp_hum fmt res.resp

  let v ?(version = (1, 1)) ~body headers status =
    let body = stream_to_body body in
    let resp =
      { Cohttp.Response.headers
      ; version =
          ( match version with
          | 1, 0 ->
              `HTTP_1_0
          | 1, 1 ->
              `HTTP_1_1
          | a, b ->
              raise
                (Invalid_argument
                   (Fmt.strf "Request.v: invalid version %d.%d" a b)) )
      ; encoding = Cohttp.Transfer.Chunked
      ; status = Cohttp.Code.status_of_code status
      ; flush = true }
    in
    {resp; body}

  let body {body; _} =
    let with_off_and_len = function
      | Some s ->
          Some (Cstruct.of_string s, 0, String.length s)
      (* XXX(dinosaure): we can optimize (avoid the [Cstruct.of_string]) this
         mapper. *)
      | None ->
          None
    in
    let open Lwt.Infix in
    match body with
    | `Empty ->
        fun () -> Lwt.return None
    | `Stream s ->
        fun () -> Lwt_stream.get s >|= with_off_and_len
    | `String s ->
        let stream = Lwt_stream.of_list [s] in
        fun () -> Lwt_stream.get stream >|= with_off_and_len
    | `Strings l ->
        let stream = Lwt_stream.of_list l in
        fun () -> Lwt_stream.get stream >|= with_off_and_len

  let headers {resp; _} = Cohttp.Response.headers resp

  let status {resp; _} =
    Cohttp.Code.code_of_status resp.Cohttp.Response.status
end

module Client (CON : Conduit_mirage.S) = struct
  open Lwt.Infix

  type t = Resolver_lwt.t * CON.t

  let request ((resolver, conduit) : t) (request : req) =
    let uri = request.uri in

    let default_header = match Uri.host uri with
      | None -> HTTP.Headers.empty
      | Some host ->Cohttp.Header.of_list ["Host",host]
    in
    let headers = Cohttp.Request.headers request.req in
    let headers = HTTP.Headers.merge default_header headers in
    let ctx = Cohttp_mirage.Client.ctx resolver conduit in
    let meth = Cohttp.Request.meth request.req in
    let body_if_redirection, push = Lwt_stream.create () in
    let body =
      match request.body with
      | None ->
          None
      | Some stream ->
          Lwt_stream.from stream
          |> Lwt_stream.map (fun (buf, off, len) ->
                 let s = Cstruct.to_string (Cstruct.sub buf off len) in
                 push (Some s); s )
          |> fun stream -> Some (`Stream stream)
    in
    (* XXX(dinosaure): [~chunked:false] is mandatory, I don't want to explain
         why (I lost one day to find this bug) but believe me. *)
    Cohttp_mirage.Client.call ~ctx ~headers ?body ~chunked:false meth uri
    >>= fun ((resp, _) as v) ->
    if
      Cohttp.Code.is_redirection
        (Cohttp.Code.code_of_status (Cohttp.Response.status resp))
    then (
      let uri' =
        Cohttp.Response.headers resp
        |> Cohttp.Header.to_list
        |> List.assoc "location"
        |> Uri.of_string
      in
      push None;
      Cohttp_mirage.Client.call ~ctx ~headers
        ~body:(`Stream body_if_redirection) ~chunked:false meth uri'
      >|= fun (resp, body) -> {resp; body} )
    else Lwt.return {resp; body = snd v}

  let connect resolver conduit = Lwt.return (resolver, conduit)
end

module Server (CON : Conduit_mirage.S) = struct
  module Cohttp_server = Cohttp_mirage.Server_with_conduit
  module IO = Cohttp_server.IO

  open Lwt.Infix

  type connection_handler = Cohttp_server.t

  type t = Conduit_mirage.server -> connection_handler -> unit Lwt.t

  type ic = IO.ic

  type oc = IO.oc

  type error = unit

  type managed = ic -> oc -> unit io

  type reqd = req * Cohttp_server.conn

  type response =
    [ `Response of resp
    | `Expert of resp * managed ]

  type error_handler = error -> unit io

  type request_handler = reqd -> response Lwt.t

  let get_request (req, _) = req

  let map_to_response_action = function
    | `Response resp ->
        let response = resp.resp in
        let body = resp.body in
        Lwt.return (`Response (response, body))
    | `Expert (resp, manager) ->
        let response = resp.resp in
        Lwt.return (`Expert (response, manager))

  let to_cohttp_request_handler (request_handler : request_handler)
      (conn : Cohttp_server.conn) (request : Cohttp.Request.t)
      (body : Cohttp_lwt.Body.t) : Cohttp_server.response_action Lwt.t =
    let host =
      match Cohttp.Header.get request.headers "Host" with
      | None -> ""
      | Some x -> x
    in
    let uri = Uri.make ~host ~path:request.resource () in
    let req = {req = request; uri; body = Some (body_to_stream body)} in
    request_handler (req, conn) >>= map_to_response_action

  let create_connection_handler request_handler _error_handler =
    let callback = to_cohttp_request_handler request_handler in
    Lwt.return
      (Cohttp_mirage.Server_with_conduit.make_response_action ~callback ())

  let listen t server handler = t server handler

  let connect = Cohttp_server.connect
end

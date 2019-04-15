type +'a io = 'a Lwt.t

type raw = Cstruct.t

type uri = Uri.t

type body = unit -> (raw * int * int) option Lwt.t

type req =
  { req : Httpaf.Request.t
  ; uri : uri
  ; body : body option }

type resp =
  { resp : Httpaf.Response.t
  ; body : body }

module HTTP = struct
  type headers = Httpaf.Headers.t

  type meth =
    [ Httpaf.Method.t
    | `PATCH ]

  type version = int * int

  let v1_0 = {Httpaf.Version.major = 1; minor = 0}
  let v1_1 = {Httpaf.Version.major = 1; minor = 1}

  module Headers = struct
    type name = string

    let name = String.lowercase_ascii

    let name_equal a b = String.equal a b

    let empty = Httpaf.Headers.empty

    let is_empty _ = assert false

    (* TODO *)
    let find name headers = Httpaf.Headers.get headers name

    let def name value headers =
      let values = Astring.String.cuts ~empty:false ~sep:"," value in
      Httpaf.Headers.add_multi headers [(name, values)]

    let def_multi name values headers =
      if List.length values = 0 then
        raise (Invalid_argument "HTTP.Headers.def_multi: empty values");
      Httpaf.Headers.add_multi headers [(name, values)]

    let get x headers =
      match find x headers with
      | Some v ->
          v
      | None ->
          raise (Invalid_argument "HTTP.Headers.get: invalid name")

    let pp = Httpaf.Headers.pp_hum

    let user_agent = "User-Agent"

    let content_type = "Content-Type"

    let access_control_allow_origin = "Access-Control-Allow-Origin"

    let access_control_allow_methods = "Access-Control-Allow-Methods"

    let access_control_allow_headers = "Access-Control-Allow-Headers"

    let of_list lst =
      List.map (fun (a,b) -> (name a, b)) lst
      |> Httpaf.Headers.of_list

    let merge a b =
      Httpaf.Headers.fold
        ~f:(fun key value acc ->
          Httpaf.Headers.add_unless_exists acc key value )
        ~init:a b
  end
end

type status = int

let s100_continue = 100

let s200_ok = 200

let to_local (ext : HTTP.meth) =
  match ext with
  | `PATCH ->
      failwith "Undefined."
  | ( `CONNECT
    | `DELETE
    | `GET
    | `HEAD
    | `OPTIONS
    | `Other _
    | `POST
    | `PUT
    | `TRACE ) as x ->
      x

let to_global = function
  | ( `CONNECT
    | `DELETE
    | `GET
    | `HEAD
    | `OPTIONS
    | `Other _
    | `POST
    | `PUT
    | `TRACE ) as x ->
      x

module Request = struct
  let headers {req; _} = req.headers

  let with_headers x headers = {x with req = {x.req with headers}}

  let with_body (x : req) body = {x with body = Some body}

  let with_uri (x : req) uri =
    let req = {x.req with target = Uri.path uri} in
    let x = {x with req} in
    {x with uri}

  let v ?(version = (1, 1)) meth ~path ?body headers =
    let req =
      { Httpaf.Request.headers
      ; meth = to_local meth
      ; target = Uri.path path
      ; version =
          ( match version with
          | 1, 0 -> HTTP.v1_0
          | 1, 1 -> HTTP.v1_1
          | a, b ->
              raise
                (Invalid_argument
                   (Fmt.strf "Request.v: invalid version %d.%d" a b)) ) }
    in
    {req; uri=path; body}

  let body ({body; _} : req) = body

  let uri {req; _} = Uri.of_string req.target

  let meth {req; _} = to_global req.meth
end

module Response = struct
  let with_headers x headers =
    {x with resp = {x.resp with Httpaf.Response.headers}}

  let with_status x status =
    let status = Httpaf.Status.of_code status in
    {x with resp = {x.resp with status}}

  let with_body (x : resp) body =
    {x with body}

  let pp fmt res = Httpaf.Response.pp_hum fmt res.resp

  let reason_of_status = function
  | 101 -> "Switching Protocols"
  | 404 -> "Not found"
  | 500 -> "Internal server error"
  | _ -> ""

  let v ?(version = (1, 1)) ~body headers status =
    let resp =
      { Httpaf.Response.headers
      ; version =
          ( match version with
          | 1, 0 -> HTTP.v1_0
          | 1, 1 -> HTTP.v1_1
          | a, b ->
              raise
                (Invalid_argument
                   (Fmt.strf "Request.v: invalid version %d.%d" a b)) )
      ; reason = reason_of_status status
      ; status = Httpaf.Status.of_code status
      }
    in
    {resp; body}

  let body {body; _} = body

  let headers {resp; _} = resp.headers

  let status {resp; _} = Httpaf.Status.to_code resp.status
end

module Client (CON : Conduit_mirage.S) = struct
  open Lwt.Infix

  type t = Resolver_lwt.t * CON.t

  module HTTPClient = Httpaf_mirage.Client(Conduit_mirage.Flow)

  let request ((resolver, conduit) : t) (request : req) =
    Printf.printf "Resolving..%!\n";
    Resolver_lwt.resolve_uri ~uri:request.uri resolver
    >>= fun endpoint ->
    Printf.printf "Resolved! Connecting..%!\n";
    Conduit_mirage.client endpoint
    >>= fun client ->
    CON.connect conduit client
    >>= fun flow ->
    Printf.printf "Connected!%!\n";
    let resp_body_stream, body_push = Lwt_stream.create () in
    let response_mailbox, notify = Lwt.wait () in
    let error_handler _ = Printf.printf "Error handler.%!\n"
    and response_handler resp r_body =
      Lwt.wakeup_later notify resp;
      let rec loop () =
        Httpaf.Body.schedule_read r_body
          ~on_eof:(fun () -> body_push None)
          ~on_read:(fun x ~off ~len ->
            let raw = Cstruct.of_bigarray x in
            body_push (Some (raw, off, len));
            loop () )
      in
      loop ()
    in
    let body =
      HTTPClient.request flow request.req ~error_handler
        ~response_handler
    in
    (* Write request body*)
    let body_stream =
      match (request.body : body option) with
      | None ->
          Lwt.return (Httpaf.Body.close_writer body)
      | Some stream -> (
          Lwt_stream.from stream
          |> Lwt_stream.iter (fun (buf, off, len) ->
                 let s = Cstruct.to_string (Cstruct.sub buf off len) in
                 Httpaf.Body.write_string body s )
          >>= function
          | () ->
              Lwt.return (Httpaf.Body.close_writer body) )
    in
    body_stream
    >>= fun () ->
    response_mailbox
    >>= fun response ->
    let unbox_body_stream () = Lwt_stream.get resp_body_stream in
    Lwt.return {resp = response; body = unbox_body_stream}

  let connect resolver conduit = Lwt.return (resolver, conduit)
end

let body_read_stream body () =
  let wait, notify = Lwt.wait () in
  Httpaf.Body.schedule_read body
    ~on_eof:(fun () -> Lwt.wakeup notify None)
    ~on_read:(fun x ~off ~len ->
      let raw = Cstruct.of_bigarray x in
      Lwt.wakeup notify (Some (raw, off, len)) );
  wait

module Server (CON : Conduit_mirage.S) = struct
  open Lwt.Infix

  module Channel = Mirage_channel_lwt.Make(Conduit_mirage.Flow)
  module IO = Mirage_http.IO.Make(Channel)

  type flow = Conduit_mirage.Flow.flow

  type connection_handler = flow -> unit Lwt.t

  type t = Conduit_mirage.server -> connection_handler -> unit Lwt.t

  type ic = IO.ic

  type oc = IO.oc

  type error = unit

  type managed = ic -> oc -> unit io

  type reqd = Httpaf.Reqd.t

  type response =
    [ `Response of resp
    | `Expert of resp * managed ]

  type error_handler = error -> unit io

  type request_handler = reqd -> response Lwt.t

  let get_request reqd =
    let req = Httpaf.Reqd.request reqd in
    let uri = Uri.of_string req.target in
    { req
    ; uri
    ; body = Some (body_read_stream (Httpaf.Reqd.request_body reqd)) }

  let respond flow reqd = function
    | `Response resp ->
        let body = Httpaf.Reqd.respond_with_streaming ~flush_headers_immediately:false reqd resp.resp in
        let rec loop () =
          resp.body () >>= function
            | None -> Httpaf.Body.flush body (fun () -> Httpaf.Body.close_writer body);
                      Lwt.return ()
            | Some (c,off,len) ->
              Httpaf.Body.write_string body (Cstruct.to_string c) ~off ~len;
              loop ()
        in loop ()
    | `Expert (resp, handler) ->
      let request_body = Httpaf.Reqd.request_body reqd in
      let write_body = Httpaf.Reqd.respond_with_streaming ~flush_headers_immediately:true reqd resp.resp in

      let upgrade_finished, notify_upgrade_finished = Lwt.wait () in

      let rec on_read _ ~off:_  ~len:_  =
        Httpaf.Body.schedule_read request_body ~on_read ~on_eof
      and on_eof () =
        Httpaf.Body.flush write_body (fun () ->
          let chan = Channel.create flow in
          Lwt.async (fun () -> handler chan chan);
          Lwt.wakeup_later notify_upgrade_finished ())
      in
      Httpaf.Body.schedule_read request_body ~on_eof ~on_read;
      upgrade_finished

  let to_httapf_request_handler flow request_handler (reqd : reqd) =
    Lwt.async (fun () -> request_handler reqd >>= fun response -> respond flow reqd response)

  let to_error_handler handler ?request _ _ = ignore request;
    Lwt.async (fun () -> handler ())

  module HTTPServer = Httpaf_mirage.Server(Conduit_mirage.Flow)

  let create_connection_handler request_handler error_handler =
    Lwt.return (fun flow ->
      let error_handler = to_error_handler error_handler in
      let request_handler = to_httapf_request_handler flow request_handler in
      (HTTPServer.create_connection_handler ~request_handler ~error_handler) flow)
  let listen t server handler = t server handler

  let listen_internal handler flow =
    Lwt.finalize
      (fun () -> handler flow)
      (fun () -> Conduit_mirage.Flow.close flow)

  let connect conduit =
    let listen s f = Conduit_mirage.listen conduit s (listen_internal f) in
    Lwt.return listen
end

type +'a io = 'a Lwt.t
type raw = Cstruct.t
type uri = Uri.t

type body = unit -> (raw * int * int) option Lwt.t

type req =
  { req: Httpaf.Request.t
  ; body: body option}

type resp =
  { resp: Httpaf.Response.t
  ; body: body}

module HTTP = struct
  type headers = Httpaf.Headers.t
  type path = string list
  type meth = [ Httpaf.Method.t
              | `PATCH ]


  type version = int * int

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
      Httpaf.Headers.add_multi headers [name,values]

    let def_multi name values headers =
      if List.length values = 0 then
        raise (Invalid_argument "HTTP.Headers.def_multi: empty values") ;
      Httpaf.Headers.add_multi headers [name,values]

    let get x headers =
      match find x headers with
      | Some v -> v
      | None -> raise (Invalid_argument "HTTP.Headers.get: invalid name")

    let pp = Httpaf.Headers.pp_hum
    let user_agent = "User-Agent"
    let content_type = "Content-Type"
    let access_control_allow_origin = "Access-Control-Allow-Origin"
    let access_control_allow_methods = "Access-Control-Allow-Methods"
    let access_control_allow_headers = "Access-Control-Allow-Headers"

    let merge a b =
      Httpaf.Headers.fold
        ~f:(fun key value acc -> Httpaf.Headers.add_unless_exists acc key value)
        ~init:a b
  end
end

type status = int

let s100_continue = 100
let s200_ok = 200

let to_local (ext:HTTP.meth) = match ext with
  | `PATCH -> failwith "Undefined."
  | (`CONNECT
  | `DELETE
  | `GET
  | `HEAD
  | `OPTIONS
  | `Other _
  | `POST
  | `PUT
  | `TRACE) as x -> x

let to_global = function
  |(`CONNECT
  | `DELETE
  | `GET
  | `HEAD
  | `OPTIONS
  | `Other _
  | `POST
  | `PUT
  | `TRACE) as x -> x

module Request = struct

  let headers {req; _} = req.headers
  let with_headers x headers = {x with req= {x.req with headers}}

  let with_path x path =
    {x with req= {x.req with target= String.concat "/" path}}

  let with_body (x : req) body = {x with body=(Some body)}

  let v ?(version = 1, 1) meth ~path ?body headers =
    let req =
      { Httpaf.Request.headers
      ; meth=(to_local meth)
      ; target= String.concat "/" path
      ; version=
          ( match version with
          | 1, 0 -> { major = 1; minor = 0}
          | 1, 1 -> { major = 1; minor = 1}
          | a, b ->
              raise
                (Invalid_argument
                   (Fmt.strf "Request.v: invalid version %d.%d" a b)) )
      }
    in
    {req; body}

  let body ({body; _} : req) = body
  let uri {req; _} = Uri.of_string req.target
  let meth {req; _} = to_global req.meth
end

module Response = struct

  let body {body; _} = body

  let headers {resp; _} = resp.headers

  let status {resp; _} = Httpaf.Status.to_code resp.status
end


module Client (CON: Conduit_mirage.S) = struct
  open Lwt.Infix
  type t = Resolver_lwt.t * CON.t

  let request ((resolver, conduit) : t) uri (request : req) =
    Printf.printf "Resolving..%!\n";
    Resolver_lwt.resolve_uri ~uri resolver >>= fun endpoint ->
    Printf.printf "Resolved! Connecting..%!\n";
    Conduit_mirage.client endpoint >>= fun client ->
    CON.connect conduit client >>= fun flow ->
    Printf.printf "Connected!%!\n";

    let resp_body_stream, body_push = Lwt_stream.create () in
    let response_mailbox, notify = Lwt.wait () in
    let error_handler _ =  Printf.printf "Error handler.%!\n"
    and response_handler resp r_body =
      Lwt.wakeup_later notify resp;
      let rec loop () =
        Httpaf.Body.schedule_read r_body
        ~on_eof:(fun () ->
            body_push None)
        ~on_read:(fun x ~off ~len ->
          let raw = Cstruct.of_bigarray x in
          body_push
            (Some (raw, off, len));
          loop ()
          )
      in loop ()
    in
    let body = Httpaf_mirage.Client.request flow request.req
                ~error_handler ~response_handler
    in
    (* Write request body*)
    let body_stream = match (request.body : body option) with
      | None -> Lwt.return (Httpaf.Body.close_writer body)
      | Some stream ->
          Lwt_stream.from stream
          |> Lwt_stream.iter (fun (buf, off, len) ->
            let s = Cstruct.to_string (Cstruct.sub buf off len) in
            Httpaf.Body.write_string body s
          )
          >>= function () -> Lwt.return (Httpaf.Body.close_writer body)
    in
    body_stream >>= fun () ->
    response_mailbox >>= fun response ->
    let unbox_body_stream () = Lwt_stream.get resp_body_stream in
    Lwt.return
    { resp=response
    ; body=unbox_body_stream
    }

  let connect resolver conduit = Lwt.return (resolver,conduit)
end

let body_read_stream body () =
  let wait, notify = Lwt.wait () in
  Httpaf.Body.schedule_read body
      ~on_eof:(fun () -> Lwt.wakeup notify None)
      ~on_read:(fun x ~off ~len ->
        let raw = Cstruct.of_bigarray x in
        Lwt.wakeup notify (Some (raw, off, len))
  ); wait

module Server (CON: Conduit_mirage.S) = struct
  open Lwt.Infix

  type connection_handler = (Conduit_mirage.Flow.flow -> unit Lwt.t)
  type t = Conduit_mirage.server ->  connection_handler -> unit Lwt.t

  type error = [`oui ]
  type flow = Conduit_mirage.Flow.flow
  type reqd = flow Httpaf.Reqd.t

  let get_request reqd = { req = Httpaf.Reqd.request reqd
                         ; body = Some (body_read_stream (Httpaf.Reqd.request_body reqd))}

  let respond reqd = function
    | `Response resp ->
    | `Expert (resp, manager) -> failwith "not implemented"

  let to_httapf_request_handler request_handler =
    fun (reqd : reqd) ->
      Lwt.async (fun () -> request_handler reqd)

  let create_connection_handler request_handler error_handler =
    let request_handler = to_httapf_request_handler request_handler in
    Lwt.return
    (Httpaf_mirage.Server.create_connection_handler
      ~request_handler
      ~error_handler
      ())

  let listen t server handler = t server handler

  let listen_internal handler flow =
      Lwt.finalize
        (fun () -> handler flow)
        (fun () -> Conduit_mirage.Flow.close flow)

  let connect conduit =
    let listen s f = Conduit_mirage.listen conduit s (listen_internal f) in
    Lwt.return listen
end
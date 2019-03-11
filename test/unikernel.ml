open Lwt.Infix

let domain = "perdu.com"

let uri = Uri.of_string "http://perdu.com/"

let ns = "8.8.8.8"

module Make (RES : Resolver_lwt.S) (CON : Conduit_mirage.S) = struct
  module HTTP_Make (HTTP : Mirage_http.S) = struct
    include HTTP
  end

  module HTTP = HTTP_Make (Mirage_http_httpaf)
  module HTTP_Client = HTTP.Client (CON)

  let start resolver conduit =
    HTTP_Client.connect resolver conduit
    >>= fun t ->
    HTTP_Client.request t
      (HTTP.Request.v `GET ~path:uri
         HTTP.HTTP.Headers.(empty |> def (name "Host") domain))
    >>= fun resp ->
    Printf.printf "resp %d%!\n" (HTTP.Response.status resp);
    Lwt_stream.from (HTTP.Response.body resp)
    |> Lwt_stream.iter (fun (x, ofs, len) ->
           Printf.printf "Line: %s\n"
             (Cstruct.to_string (Cstruct.sub x ofs len)) )
end

module MakeServer (CON : Conduit_mirage.S) = struct
  module HTTP_Make (HTTP : Mirage_http.S) = struct
    include HTTP
  end

  module HTTP = HTTP_Make (Mirage_http_cohttp)
  module Server = HTTP.Server (CON)

  let body () =
    let stream =
      Lwt_stream.of_list ["Tres bien."; "OK\n"]
      |> Lwt_stream.map (fun str ->
             (Cstruct.of_string str, 0, String.length str) )
    in
    fun () -> Lwt_stream.get stream

  let error_handler () = Printf.printf "Error%!\n"; Lwt.return_unit

  let request_handler _ =
    let body = body () in
    Printf.printf "request handler%!\n";
    let headers = HTTP.HTTP.Headers.(def (name "Content-length") "13" empty) in
    let resp = HTTP.Response.v ~body headers 200 in
    let response = `Response resp in
    Lwt.return response

  let start conduit =
    Server.connect conduit
    >>= fun t ->
    Server.create_connection_handler request_handler error_handler
    >>= fun conn_handler -> Server.listen t (`TCP 8080) conn_handler
end

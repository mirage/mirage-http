type 'a io = 'a Lwt.t
type raw = Cstruct.t
type uri = Uri.t

type req
type resp
include
  Mirage_http.S
  with type req := req
   and type resp := resp
   and type 'a io := 'a io
   and type raw := raw
   and type uri := uri
   and type HTTP.headers = Cohttp.Header.t
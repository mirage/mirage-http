open Mirage

let client =
  let packages = [package ~sublibs:["httpaf";"cohttp"] "mirage-http"] in
  foreign ~packages "Unikernel.Make" @@ resolver @-> conduit @-> job

let server =
  let packages = [package ~sublibs:["httpaf";"cohttp"] "mirage-http"] in
  foreign ~packages "Unikernel.MakeServer" @@ conduit @-> job

let () =
  let stack = generic_stackv4 default_network in
  let res_dns = resolver_dns stack in
  let conduit = conduit_direct ~tls:false stack in
  let job = [server $ conduit] in
  register "http-fetch" job

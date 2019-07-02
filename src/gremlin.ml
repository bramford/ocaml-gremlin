module Websocket = struct
  let new_connection uri =
    let%lwt endp = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
    let%lwt client = Conduit_lwt_unix.(endp_to_client ~ctx:default_ctx endp) in
    let%lwt recv, send = Websocket_lwt_unix.with_connection ~ctx:Conduit_lwt_unix.default_ctx client uri in
    Lwt.return (recv, send)

end

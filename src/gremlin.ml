module Websocket = struct
  let new_connection uri =
    let%lwt endp = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
    let%lwt client = Conduit_lwt_unix.(endp_to_client ~ctx:default_ctx endp) in
    let%lwt recv, send = Websocket_lwt_unix.with_connection ~ctx:Conduit_lwt_unix.default_ctx client uri in
    Lwt.return (recv, send)

  type op_processor =
    | Session of string
    | Standard

  type message_status =
    | Response_invalid of response_invalid
    | Request_failed of request_failed
    | Unmatched_request_id
    | Good of Yojson.Basic.t

  and request_failed =
    | Non_200_status_code of Yojson.Basic.t

  and response_invalid =
    | No_status_field
    | Missing_fields
    | Missing_status_fields
    | Missing_field of string
    | Invalid_status_json_type of Yojson.Basic.t
    | Invalid_result_json_type of Yojson.Basic.t
    | Invalid_result_field
    | Invalid_request_id_json_type of Yojson.Basic.t
    | Result_fields_wrong
    | Json_parse_failure
    | Unknown_json_parse_failure

end

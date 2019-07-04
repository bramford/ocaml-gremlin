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

  let check_result = function
    | Good m as message ->
      begin try
        let result = Yojson.Basic.Util.member "result" m in
        if (CCList.equal
             (String.equal)
             (CCList.sort (String.compare) (Yojson.Basic.Util.keys result))
             (CCList.sort (String.compare) ["data";"meta"])
        ) then
          message
        else (
          List.iter (Printf.printf "Result key: %s\n%!") (Yojson.Basic.Util.keys result);
          Response_invalid Result_fields_wrong
        )
      with
        | _ -> Response_invalid (Missing_field "result")
        end
    | _ as status -> status

  let check_fields = function
    | Good m as message ->
      if (CCList.equal
           (String.equal)
           (Yojson.Basic.Util.keys m)
           ["requestId";"status";"result"]
      ) then
        message
      else (
        List.iter (Printf.printf "Field key: %s\n%!") (Yojson.Basic.Util.keys m);
        Response_invalid Missing_fields
      )
    | _ as status -> status

  let check_status = function
    | Good m as message ->
      begin match Yojson.Basic.Util.member "status" m with
        | `Null -> Response_invalid (Missing_field "status")
        | _ as status ->
          if (CCList.equal
               (String.equal)
               (CCList.sort (String.compare) (Yojson.Basic.Util.keys status))
               (CCList.sort (String.compare) ["code";"attributes";"message"])
          ) then (
            begin match
                Yojson.Basic.Util.member "code" status
            with
            | `Int i ->
              begin match i with
                | 200 -> message
                | _ -> Request_failed (Non_200_status_code (Yojson.Basic.Util.member "status" m))
              end
            | json -> Response_invalid (Invalid_status_json_type json)
            end
          ) else (
            Response_invalid Missing_status_fields
          )
      end
    | _ as status -> status

  let check_request_id request_id = function
    | Good m as message ->
      begin match Yojson.Basic.Util.member "requestId" m with
        | `String s ->
          if String.equal s request_id then
            message
          else
            Unmatched_request_id
      | json -> Response_invalid (Invalid_request_id_json_type json)
      end
    | _ as status -> status
end

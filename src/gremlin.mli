module Websocket : sig
  val new_connection :
    Uri.t ->
    ((unit -> Websocket.Frame.t Lwt.t) * (Websocket.Frame.t -> unit Lwt.t))
    Lwt.t
end

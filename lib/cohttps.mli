val foo : string

module IO : Cohttp.S.IO
 with type 'a t = 'a Lwt.t
 and type ic = Lwt_io.input_channel
 and type oc = Lwt_io.output_channel
 and type conn = Tls_lwt.Unix.t


module Net : Cohttp_lwt.Net

module Client : Cohttp_lwt.S.Client
module Server : Cohttp_lwt.S.Server

(*
module Irmin_server : Irmin_http_server.S
   *)

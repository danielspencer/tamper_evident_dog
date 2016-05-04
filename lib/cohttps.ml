let foo = "asdf"

module IO = struct

  (** ['a t] represents a blocking monad state *)
  type +'a t = 'a Lwt.t

  (** [a >>= b] will pass the result of [a] to the
      [b] function.  This is a monadic [bind]. *)
  let (>>=) = Lwt.(>>=)


  (** [return a] will construct a constant IO value. *)
  let return = Lwt.return

  (** [ic] represents an input channel *)
  type ic = Lwt_io.input_channel

  (** [oc] represents an output channel *)
  type oc = Lwt_io.output_channel

  (** [conn] represents the underlying network flow *)
  type conn = Tls_lwt.Unix.t

  (** [read_line ic] will read a single line terminated
      by CR or CRLF from the input channel [ic].  It returns
      {!None} if EOF or other error condition is reached. *)
  let read_line = Lwt_io.read_line_opt

  (** [read ic len] will block until a maximum of [len] characters
      are read from the input channel [ic].  It returns an
      empty string if EOF or some other error condition occurs
      on the input channel, and can also return fewer than [len]
      characters if input buffering is not sufficient to satisfy the
      request. *)
  let read ic count = Lwt_io.read ic ~count

  (** [write oc s] will block until the complete [s] string is
      written to the output channel [oc]. *)
  let write oc str = Lwt_io.write oc str

  (** [flush oc] will return when all previously buffered content
      from calling {!write} have been written to the output channel
      [oc]. *)
  let flush oc = Lwt_io.flush oc
end


module Net = struct
  open Lwt
  module IO = IO
  type ctx = Tls.Config.client
  let sexp_of_ctx = Tls.Config.sexp_of_client
  let default_ctx = Tls.Config.client ~authenticator:X509.Authenticator.null ()

  let connect_uri ~ctx uri =
    let host = match Uri.host uri with None -> assert false | Some h -> h in
    let port = match Uri.port uri with None -> assert false | Some p -> p in
    let conn =
      Tls_lwt.Unix.connect ctx (host, port)
    in
    conn >>= fun conn ->
        let ic, oc = Tls_lwt.of_t conn in
        Lwt.return (conn, ic, oc)

  let close c = Lwt.catch (fun () -> Lwt_io.close c) (fun _ -> return_unit)

  let close_in ic = ignore_result (close ic)

  let close_out oc = ignore_result (close oc)

  let close ic oc = ignore_result (close ic >>= fun () -> close oc)

end

module Client = Cohttp_lwt.Make_client (IO) (Net)
module Server = struct
  include Cohttp_lwt.Make_server (IO)

  (** Start the server, listening at the given adress. *)

end


module Y = struct
  let pretty d =
    let tm = Unix.localtime (Int64.to_float d) in
    Printf.sprintf "%02d:%02d:%02d"
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
end
  module X = struct
    include Cohttp_lwt_unix.Server
    let listen t ?timeout uri =
      let port = match Uri.port uri with
        | None   -> 8080
        | Some p -> p in
      create ?timeout ~mode:(`TCP (`Port port)) t
  end


module Make = Irmin_http_server.Make (X)(Y)


(*
module Make_server = Irmin_http_server.Make (Server)
    Irmin_http_server.Make
   *)

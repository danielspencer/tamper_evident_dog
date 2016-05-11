open Lwt.Infix
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
  let default_ctx =
    Tls.Config.client ~authenticator:X509.Authenticator.null ()

  let connect_uri ~ctx uri =
    let host = match Uri.host uri with None -> assert false | Some h -> h in
    let port = match Uri.port uri with None -> 443          | Some p -> p in
    let conn =
      Tls_lwt.Unix.connect ctx (host, port)
    in
    conn >>= fun conn ->
        let ic, oc = Tls_lwt.of_t conn in
        Lwt.return (conn, ic, oc)

  let close c = Lwt.catch
      (fun () -> Lwt_io.close c)
      (fun _ ->
         Printf.printf "CLOSING A CLIENT STREAM?\n%!";
         return_unit)

  let close_in ic = ignore_result (close ic)

  let close_out oc = ignore_result (close oc)

  let close ic oc = ignore_result (close ic >>= fun () -> close oc)

end

module Client = Cohttp_lwt.Make_client (IO) (Net)
module Server = Cohttp_lwt.Make_server (IO)



module Y = struct
  let pretty d =
    let tm = Unix.localtime (Int64.to_float d) in
    Printf.sprintf "%02d:%02d:%02d"
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
end

let safely th =
  Lwt.catch (fun () -> th >>= fun _ -> Lwt.return_unit) (fun _ -> Lwt.return_unit)

let accept_fine ?trace conf fd =
  Printf.printf "Initialised accept\n"; flush stdout;
  Lwt_unix.accept fd >>= fun (fd', addr) ->
  Printf.printf "Accepted\n"; flush stdout;
  Lwt.catch (fun () ->
      Tls_lwt.Unix.server_of_fd conf ?trace fd'
      >|= fun t ->
      Printf.printf "TLS server created?\n"; flush stdout;
      (t, addr))
    (fun exn ->
       safely (Lwt_unix.close fd') >>= fun () -> Lwt.fail exn)

let serve_ssl port callback =
  X509_lwt.private_of_pems
    ~cert:"../certs/example.com.crt"
    ~priv_key:"../certs/example.com.key"
  >>= fun certificate ->
  let config =
    Tls.Config.(server ~certificates:(`Single certificate) ~ciphers:Ciphers.supported ())
  in
  let server_s =
    let open Lwt_unix in
    let s = socket PF_INET SOCK_STREAM 0 in
    setsockopt s Unix.SO_REUSEADDR true ;
    bind s (ADDR_INET (Unix.inet_addr_any, port)) ;
    listen s 1000 ;
    s in
  accept_fine config server_s >>= fun (t, addr) ->
  Printf.printf "Tls session passed to callback\n"; flush stdout;
  callback t addr
  (*
  Lwt.catch
    (fun () -> callback t addr)
    (fun _ ->
       Printf.printf "AAAAAAAAAAAAAAAAAAAA\n"; flush stdout;
       Printexc.print_backtrace stdout; flush stdout; Lwt.return_unit)
         *)

module X = struct
  include Server
  let listen t ?timeout:_ uri =
    let port = match Uri.port uri with
      | None   -> 443
      | Some p -> p in
    serve_ssl port (fun tls _addr ->
        let ic, oc = Tls_lwt.of_t tls in
        callback t tls ic oc)
end

module Server_l = X


module TServer_make = Irmin_http_server.Make (X)(Y)
module TClient_make = Irmin_http.Make (Client)

module Server_make = Irmin_unix.Irmin_http_server.Make
module Client_make = Irmin_unix.Irmin_http.Make

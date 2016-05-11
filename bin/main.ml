(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Cmdliner
open Printf

(* Global options *)
type global = {
  level: Log.log_level option;
}

let app_global g =
  Log.color_on ();
  match g.level with
  | None   -> ()
  | Some d -> Log.set_log_level d

(* Help sections common to all commands *)
let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";

  `S "AUTHORS";
  `P "Thomas Gazagnaire   <thomas@gazagnaire.org>";

  `S "BUGS";
  `P "Check bug reports at https://github.com/samoht/dog/issues.";
]

let global =
  let debug =
    let doc =
      Arg.info ~docs:global_option_section ~doc:"Be very verbose." ["debug"] in
    Arg.(value & flag & doc) in
  let verbose =
    let doc =
      Arg.info ~docs:global_option_section ~doc:"Be verbose." ["v";"verbose"] in
    Arg.(value & flag & doc) in
  let level debug verbose =
    match debug, verbose with
    | true, _    -> { level = Some Log.DEBUG }
    | _   , true -> { level = Some Log.INFO }
    | _          -> { level = None } in
  Term.(pure level $ debug $ verbose)

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~doc ~man title

type command = unit Term.t * Term.info

type sub = {
  name: string;
  doc : string;
  man : Manpage.block list;
  term: unit Term.t;
}

let create_command c =
  let man = [
    `S "DESCRIPTION";
    `P c.doc;
  ] @ c.man in
  c.term, term_info c.name ~doc:c.doc ~man

(* Converters *)

let pr_str = Format.pp_print_string

let run t =
  Lwt_unix.run (
    Lwt.catch
      (fun () -> t)
      (function e -> eprintf "%s\n%!" (Printexc.to_string e); exit 1)
  )

let mk (fn:'a): 'a Term.t =
  Term.(pure (fun global -> app_global global; fn) $ global)

let cwd = Sys.getcwd ()

let root =
  let doc =
    Arg.info ~docv:"ROOT" ~doc:"The repository root." ["r";"root"] in
  Arg.(value & opt string cwd & doc)

let key n =
  let doc =
    Arg.info ~docv:"KEY" ~doc:"The key used to encrypt the secure log." [] in
  Arg.(required & pos n (some string) None & doc)

(* INIT *)
let init = {
  name = "init";
  doc  = "Initialize a client store.";
  man  = [];
  term =
    let client_name =
      let doc =
        Arg.info ~docv:"NAME" ~doc:"The client name." [] in
      Arg.(required & pos 0 (some string) None & doc)
    in
    let init root name key = run (Dog_client.init ~root ~key name) in
    Term.(mk init $ root $ client_name $ key 1)
}

(* APPEND *)
let append = {
  name = "append";
  doc  = "Append to the secure log.";
  man  = [];
  term =
    let log =
      let doc =
        Arg.info ~docv:"LOG" ~doc:"The message to log" [] in
      Arg.(required & pos 0 (some string) None & doc)
    in
    let write root log =
      run (Dog_client.write_to_log ~root log)
    in
    Term.(mk write $ root $ log);
}

(* APPEND *)
let test_append = {
  name = "test_append";
  doc  = "Append to the secure log.";
  man  = [];
  term =
    let log =
      let doc =
        Arg.info ~docv:"LOG" ~doc:"The message to log" [] in
      Arg.(required & pos 0 (some string) None & doc)
    in
    let write root log =
      run (Dog_client.test_write_to_log ~root log)
    in
    Term.(mk write $ root $ log);
}

(* DUMP *)
let dump = {
  name = "dump";
  doc  = "Dump the contents of the secure log to stdout.";
  man  = [];
  term =
    let dump root key =
      run (Dog_client.dump_log ~root key)
    in
    Term.(mk dump $ root $ key 0);
}

(* SERVER DUMP *)
let server_dump = {
  name = "serverdump";
  doc  = "Dump the contents of the secure log to stdout.";
  man  = [];
  term =
    let client_name =
      let doc =
        Arg.info ~docv:"NAME" ~doc:"The client name." [] in
      Arg.(required & pos 0 (some string) None & doc)
    in
    let dump root client_name =
      run (Dog.dump_log ~root client_name)
    in
    Term.(mk dump $ root $ client_name);
}

(* PUSH *)
let push = {
  name = "push";
  doc  = "Synchronize the client store to a Dog server.";
  man  = [];
  term =
    let msg =
      let doc =
        Arg.info ~docv:"MSG" ~doc:"The commit message." ["m"] in
      Arg.(required & opt (some string) None & doc)
    in
    let server =
      let doc =
        Arg.info ~docv:"SERVER" ~doc:"The server URI." [] in
      Arg.(required & pos 0 (some string) None & doc)
    in
    let push root msg server =
      let server = Uri.of_string server in
      run (Dog_client.push ~root ~msg server)
    in
    Term.(mk push $ root $ msg $ server);
}

(* INTERMEDIARY SERVER *)
let intermediary = {
  name = "intermediary";
  doc  = "Run an interemediary server.";
  man  = [];
  term =
    let port =
      let doc =
        Arg.info ~docv:"PORT" ~doc:"The port to listen on." [] in
      Arg.(required & pos 0 (some string) None & doc)
    in
    let server =
      let doc =
        Arg.info ~docv:"SERVER" ~doc:"The trusted server URI." [] in
      Arg.(required & pos 1 (some string) None & doc)
    in
    let listen root port server =
      let server = Uri.of_string server in
      let port = int_of_string port in
      run (Dog_intermediary.listen ~root port server)
    in
    Term.(mk listen $ root $ port $ server);
}

(* LISTEN *)
let listen = {
  name = "listen";
  doc  = "Listen for incoming client connections.";
  man  = [];
  term =
    let listen root = run (Dog.listen ~root) in
    Term.(mk listen $ root);
}

(* HELP *)
let help = {
  name = "help";
  doc  = "Display help about Dog and Dog commands.";
  man = [
    `P "Use `$(mname) help topics' to get the full list of help topics.";
  ];
  term =
    let topic =
      let doc = Arg.info [] ~docv:"TOPIC" ~doc:"The topic to get help on." in
      Arg.(value & pos 0 (some string) None & doc )
    in
    let help man_format cmds topic = match topic with
      | None       -> `Help (`Pager, None)
      | Some topic ->
        let topics = "topics" :: cmds in
        let conv, _ = Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
        match conv topic with
        | `Error e                -> `Error (false, e)
        | `Ok t when t = "topics" -> List.iter print_endline cmds; `Ok ()
        | `Ok t                   -> `Help (man_format, Some t) in
    Term.(ret (mk help $Term.man_format $Term.choice_names $topic))
}

let default =
  let doc = "Dog, A loyal and faithful synchronisation tool." in
  let man = [
    `S "DESCRIPTION";
    `P "FIXME";
  ] in
  let usage global =
    app_global global;
    printf
      "usage: dog [--version]\n\
      \           [--help]\n\
      \           <command> [<args>]\n\
      \n\
      The most commonly used subcommands are:\n\
      \    init        %s\n\
      \    push        %s\n\
      \    listen      %s\n\
      \n\
      See `dog help <command>` for more information on a specific command.\n%!"
      init.doc push.doc listen.doc in
  Term.(pure usage $ global),
  Term.info "dog"
    ~version:Version.current
    ~sdocs:global_option_section
    ~doc
    ~man

(* test *)
let test_key = {
  name = "test_key";
  doc  = "AAAAAAAA";
  man  = [];
  term =
    let test root key =
      run (Dog_client.test_key_write_read ~root key)
    in
    Term.(mk test $ root $ key 0);
}

open Lwt.Infix

let _ = Dog_intermediary.listen

       (*
let () = Log.set_log_level Log.DEBUG
          *)

(* test *)
let test_server = {
  name = "test_server";
  doc  = "AAAAAAAA";
  man  = [];
  term =
    let test () =
      run (
        let open Cohttp in
        let module Server = Cohttps.Server_l in
        let callback _conn req body =
          let uri = req |> Request.uri |> Uri.to_string in
          let meth = req |> Request.meth |> Code.string_of_method in
          let headers = req |> Request.headers |> Header.to_string in
          body |> Cohttp_lwt_body.to_string >|= (fun body ->
              (Printf.sprintf "Server speaking!\nUri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
                 uri meth headers body))
          >>= (fun body -> Server.respond_string ~status:`OK ~body ())
        in
        Server.listen (Server.make ~callback ()) (Uri.of_string "http://localhost:8080/")
      )
    in
    Term.(mk test $ const ());
}

(* test *)
let test_client = {
  name = "test_client";
  doc  = "AAAAAAAA";
  man  = [];
  term =
    let test () =
      run (
        let open Cohttp in
        let module Client = Cohttps.Client in
        Client.get (Uri.of_string "http://127.0.0.1:8080/") >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Printf.printf "Response code: %d\n" code;
        Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
        body |> Cohttp_lwt_body.to_string >|= fun body ->
        Printf.printf "Body of length: %d\n" (String.length body);
        Printf.printf "Bodt of %s\n%!" body
      )
    in
    Term.(mk test $ const ());
}

let commands = List.map create_command [
    help;
    init;
    push;
    listen;
    intermediary;
    append;
    test_append;
    server_dump;
    dump;
    test_key;
    test_server;
    test_client;
  ]

let () = Ezcmdliner.run default commands

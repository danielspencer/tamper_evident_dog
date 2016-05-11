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

module Log = Log.Make (struct let section = "Dog.client" end)
open Irmin_unix
open Sexplib.Std

let (>>=) = Lwt.bind
let (>|=) a f = Lwt.map f a

let write_key ~root key =
  Lwt_io.with_file
    ~flags:[Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY]
    ~mode:Lwt_io.output
    (Filename.concat root ".key")
    (fun channel ->
       let str =
         Cstruct.to_string key
       in
       Lwt_io.write channel str)

let read_key ~root =
  Lwt_io.with_file
    ~flags:[Unix.O_RDONLY]
    ~mode:Lwt_io.input
    (Filename.concat root ".key")
    (fun channel ->
       Lwt_io.read channel
       >|= Cstruct.of_string)

let read_log ~root store =
  Irmin.read store ["secure_log"] >>= function
  | None -> assert false
  | Some entries_sexp ->
    let entries_sexp = Sexplib.Sexp.of_string entries_sexp in
    let entries = list_of_sexp Secure_log.entry_of_sexp entries_sexp in
    read_key ~root >>= fun key ->
    let key = Secure_log.key_of_cstruct key in
    Secure_log.reconstruct key entries
    |> Lwt.return

let split_log log =
  let key = Secure_log.get_key log |> Secure_log.cstruct_of_key in
  let entries = Secure_log.get_entries log |> Sexplib.Conv.sexp_of_list Secure_log.sexp_of_entry in
  key, entries

let write_log ~root store log =
  let key, entries = split_log log in
  let entries_str = entries |> Sexplib.Sexp.to_string in
  Irmin.update store ["secure_log"] entries_str >>= fun () ->
  write_key ~root key

let init ~root ~key name =
  let key = Cstruct.of_string key in
  let head = Git.Reference.of_raw ("refs/heads/" ^ name) in
  let config = Irmin_git.config ~root ~bare:false ~head () in
  Irmin.of_tag Dog_misc.base_store config Dog_misc.task name >>= fun t ->
  Irmin.head (t "head") >>=  begin function
    | Some _ -> Lwt.return_unit
    | None   ->
      Irmin.update (t "Initial commit") [".init"] (Dog_misc.timestamp ())
  end
  >>= fun () ->
  Dog_misc.(mk_store base_store ~root) >>= fun t ->
  let store = t "secure_log" in
  Secure_log.new_log (Secure_log.key_of_cstruct key)
  |> write_log ~root store

let test_key_write_read ~root key =
  let key = Cstruct.of_string key in
  write_key ~root key >>= fun () ->
  read_key ~root >>= fun key' ->
  assert (Cstruct.equal key key')
  |> Lwt.return


let entry_type = Cstruct.create 1

let write_to_log ~root message =
  Dog_misc.(mk_store base_store ~root) >>= fun t ->
  let store = t "secure_log" in
  read_log ~root store >>= fun log ->
  let log' = Secure_log.append entry_type (Cstruct.of_string message) log in
  write_log ~root store log'

let test_write_to_log ~root message =
  Dog_misc.(mk_store base_store ~root) >>= fun t ->
  let store = t "secure_log" in
  read_log ~root store >>= fun log ->
  let log = ref log in
  for _ = 1 to 1000 do
    log := Secure_log.append entry_type (Cstruct.of_string message) !log
  done;
  write_log ~root store !log

let dump_log ~root key =
  let key = key |> Cstruct.of_string |> Secure_log.key_of_cstruct in
  Dog_misc.(mk_store base_store ~root) >>= fun t ->
  let store = t "secure_log" in
  read_log ~root store >|= fun log ->
  Secure_log.decrypt_all log key
  |> List.map Cstruct.to_string
  |> List.iteri (fun i str -> Printf.printf "%i: %s\n" i str)

let remote_store =
  Irmin.basic (module Cohttps.Client_make) (module Irmin.Contents.String)
    (*
  Irmin.basic (module Irmin_http.Make) (module Irmin.Contents.String)
       *)

let chdir dir = Unix.handle_unix_error Unix.chdir dir

let in_dir dir fn =
  let reset_cwd =
    let cwd = Unix.handle_unix_error Unix.getcwd () in
    fun () -> chdir cwd in
  chdir dir;
  try
    let r = fn () in
    reset_cwd ();
    r
  with e ->
    reset_cwd ();
    raise e

let (/) x y = x @ [y]

let list kind dir =
  in_dir dir (fun () ->
      let d = Sys.readdir (Sys.getcwd ()) in
      let d = Array.to_list d in
      List.filter kind d
    )

let files =
  list (fun f -> try not (Sys.is_directory f) with Sys_error _ -> true)

let directories =
  list (fun f -> try Sys.is_directory f with Sys_error _ -> false)

let rec_files ?(keep=fun _ -> true) root =
  let rec aux accu dir =
    let path = Dog_misc.path (root :: dir) in
    let d =
      directories path
      |> List.filter keep
      |> List.map ((/) dir)
    in
    let f =
      files path
      |> List.filter keep
      |> List.map ((/) dir)
    in
    List.fold_left aux (f @ accu) d in
  aux [] []

let keep = function
  | ".git" | ".key" -> false
  | _ -> true

let of_path p =
  let file = Dog_misc.path p in
  let ic = open_in file in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  buf

let update_files files view =
  Lwt_list.iter_s (fun path ->
      Irmin.update view path (of_path path)
    ) files

let push ~root ~msg ?watch server =
  let config = Irmin_http.config server in
  Dog_misc.(mk_store base_store ~root) >>= fun t ->
  Irmin.tag_exn (t "tag") >>= fun tag ->
  Irmin.of_tag remote_store config Dog_misc.task tag >>= fun remote ->
  let rec aux () =
    Log.debug "Aux reached\n"; flush stdout;
    let files = rec_files ~keep root in
    Irmin.with_hrw_view (t msg) `Update (update_files files) >>=
    Irmin.Merge.exn >>= fun () ->
    Log.debug "performed local store\n"; flush stdout;
    let remote = Irmin.remote_basic (remote "dog push") in
    Log.debug "Created remote\n"; flush stdout;
    Irmin.push_exn (t msg) remote >>= fun () ->
    Log.debug "Finished pushing\n"; flush stdout;
    match watch with
    | None   -> Lwt.return_unit
    | Some d ->
      Lwt_unix.sleep d >>= fun () ->
      aux ()
  in
  aux ()

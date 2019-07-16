(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Cohttp

let respond body =
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()

let error body =
  Cohttp_lwt_unix.Server.respond_string ~status:(`Code 404) ~body  ()

let process root path =
  let file = String.concat Filename.dir_sep (root :: path) in
  let file = OpamFilename.of_string file in
  if not (OpamFilename.exists file) then
    error (Printf.sprintf "%s does not exist." (OpamFilename.to_string file))
  else if Sys.is_directory (OpamFilename.to_string file) then
    respond ""
  else
    respond (OpamFilename.read file)

let make_server root port =
  Printf.printf "Starting the filer-server on port %d.\nRoot directory is %s\n%!"
    port root;
  let callback _conn_id req _body =
    let path = Uri.path (Request.uri req) in
    Printf.printf "Request received: PATH=%s\n%!" path;
    let path = Re.(split (compile (rep1 (char '/')))) path in
    let path = List.filter ((<>) "") path in
    process root path in
  let conn_closed _ = () in
  let config = Cohttp_lwt_unix.Server.make ~conn_closed ~callback () in
  let server = `TCP (`Port port) in
  Cohttp_lwt_unix.Server.create ~mode:server config

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "usage: opam-rt-server <path>\n";
    exit 2
  );
  let root = Sys.argv.(1) in
  if not (Sys.file_exists root) then (
    Printf.eprintf "%s does not exist.\n" root;
    exit 3;
  );
  if not (Sys.is_directory root) then (
    Printf.eprintf "%s is not a directory.\n" root;
    exit 4;
  );
  Lwt_main.run (make_server root 1234)

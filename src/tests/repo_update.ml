(*
 * Copyright (c) 2013-2020 OCamlPro
 * Authors Thomas Gazagnaire <thomas@gazagnaire.org>,
 *         Louis Gesbert <louis.gesbert@ocamlpro.com>,
 *         Raja Boujbel <raja.boujbel@ocamlpro.com>
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

open Utils

let name = "repo-update"

let init_u kind path =
  log "init-repo-update %s\n" (OpamFilename.Dir.to_string path);
  let { repo_name; repo_root; repo_url; opam_root; contents_root } =
    create_config kind path
  in
  OpamConsole.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo_root);
  create_repo_with_history repo_root contents_root;
  write_repo_config path repo_name repo_url;
  let stop_server = start_file_server repo_root repo_url in
  try
    OpamConsole.msg
      "Initializing an opam instance in %s/ ...\n"
      (OpamFilename.Dir.to_string opam_root);
    Opamlib.init opam_root repo_name repo_url;
    Opamlib.install opam_root
      ~version:(OpamPackage.Version.of_string "1")
      (OpamPackage.Name.of_string "a");
    stop_server ()
  with e -> stop_server (); raise e

let init kind = run (init_u kind)

(* Basic repository update test: we verify that the global contents is
   the same as the repository contents after each new commit in the
   repository + upgrade. *)
let test_u path =
  log "test-repo-update %s" (OpamFilename.Dir.to_string path);
  let { repo_root; repo_url; opam_root ; _ } = read_config path in
  let commits = List.tl @@ List.rev @@ Git.commits repo_root in
  let stop_server = start_file_server repo_root repo_url in
  (* OpamConsole.msg "Commits:\n  %s\n\n" (String.concat "\n  " commits); *)
  List.iter (fun commit ->
      OpamConsole.msg "%s\n"
        (OpamConsole.colorise `yellow @@ Printf.sprintf "*** %s ***" commit);
      Git.checkout repo_root commit;
      Git.branch repo_root;
      update_server_index repo_root repo_url;
      Opamlib.update opam_root;
      Opamlib.upgrade opam_root [];
      Check.packages repo_root opam_root;
    ) (shuffle commits);
  stop_server ()

let run _kind = run test_u

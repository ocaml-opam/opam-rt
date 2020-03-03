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

let name = "big-upgrade"

let init kind path =
  log "init-big-upgrade %s\n" (OpamFilename.Dir.to_string path);
  let { repo_name; repo_root; repo_url; opam_root; _ } =
    create_config kind path
  in
  write_repo_config path repo_name (repo_url, None);
  OpamConsole.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo_root);

  OpamFilename.mkdir repo_root;

  OpamFile.Repo.write
    (OpamFile.make OpamFilename.Op.(repo_root // "repo"))
    (OpamFile.Repo.create ~opam_version:OpamFile.Repo.format_version ());

  OpamFilename.extract_in (data "repo_packages.tar.gz") repo_root;

  let stop_server = start_file_server repo_root repo_url in
  Git.init repo_root;
  let git_dir_re = Re.(compile (str "\\(.*/\\)?\\.git\\(/.*\\)?")) in
  Git.add_list repo_root
    (OpamStd.List.filter_map (fun f ->
         if Re.(all git_dir_re (OpamFilename.to_string f)) <> [] then None
         else Some f)
        (OpamFilename.rec_files repo_root));
  Git.commit repo_root "Init repo from stored data";
  Git.branch repo_root;
  OpamConsole.msg
    "Initializing an opam instance in %s/ ...\n"
    (OpamFilename.Dir.to_string opam_root);
  Opamlib.init opam_root repo_name repo_url;
  Opamlib.import opam_root ~fake:true (data "init.export");
  stop_server ()

let check_export opam_root reference =
  let exportfile =
    OpamFilename.of_string (OpamSystem.temp_file "opam-rt-export")
  in
  Opamlib.export opam_root exportfile;
  let ret =
    OpamProcess.run @@
    OpamProcess.command "diff"
      (List.map OpamFilename.to_string [reference; exportfile]) in
  if OpamProcess.check_success_and_cleanup ret then
    (OpamConsole.msg "%s Export files matches reference\n"
       (OpamConsole.colorise `green "[OK]"))
  else
    (OpamConsole.msg "%s Export file differs from %s\n"
       (OpamConsole.colorise `red "[FAIL]")
       (OpamFilename.to_string reference);
     failwith "Installed packages don't match expectations")


let run _kind path =
  log "test-big-upgrade %s" (OpamFilename.Dir.to_string path);
  let { repo_root; repo_url; opam_root; _ } = read_config path in
  let step = step () in
  let stop_server = start_file_server repo_root repo_url in
  step "update";
  Opamlib.update opam_root;
  check_export opam_root (data "init.export");
  step "upgrade";
  Opamlib.upgrade opam_root ~fake:true [];
  check_export opam_root (data "expected.export");
  stop_server ()

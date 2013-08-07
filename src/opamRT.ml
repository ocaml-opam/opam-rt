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

open OpamFilename.OP
open OpamRTcommon
open OpamTypes

let log fmt =
  OpamGlobals.log "RT" fmt

let ok () =
  OpamGlobals.msg "%s\n%!" (Color.green "[SUCCESS]")

let error e =
  OpamGlobals.msg "%s\n%s\n%!"
    (Printexc.to_string e)
    (Color.red "[ERROR]")

let newline () =
  OpamGlobals.msg "\n"

let run f x =
  let seed = OpamRTcommon.seed () in
  Random.init seed;
  try
    OpamGlobals.msg "SEED %d\n" seed;
    f x;
    newline ();
    ok ()
  with e ->
    newline ();
    error e

type config = {
  repo         : repository;
  opam_root    : dirname;
  contents_root: dirname;
}

let create_config kind path =
  if OpamFilename.exists_dir path then
    OpamGlobals.error_and_exit "%s already exists." (OpamFilename.Dir.to_string path);
  OpamFilename.mkdir path;
  let repo_name = OpamRepositoryName.of_string "base" in
  let repo_root = path / "repo" in
  let opam_root = path / "opam" in
  let contents_root = path / "contents" in
  let repo_address = match kind with
    | Some `git   -> OpamFilename.Dir.to_string repo_root, Some Git.test_tag
    | Some `http  -> "http://127.0.0.1:1234", None
    | Some `local
    | None        -> OpamFilename.Dir.to_string repo_root, None
    | _           -> failwith "TODO" in
  let repo_kind = guess_repository_kind kind repo_address in
  let repo = {
    repo_name;
    repo_root;
    repo_priority = 0;
    repo_address;
    repo_kind;
  } in
  { repo; opam_root; contents_root }

let read_config path =
  if not (OpamFilename.exists_dir path) then
    OpamGlobals.error_and_exit "opam-rt has not been initialized properly";
  let repo =
    let root = path / "repo" in
    let repo = OpamRepository.local root in
    OpamFile.Repo_config.read (OpamPath.Repository.config repo) in
  let opam_root = path / "opam" in
  let contents_root = path / "contents" in
  { repo; opam_root; contents_root }

let update_server_index repo =
  match repo.repo_kind with
  | `http -> OpamFilename.exec repo.repo_root [["opam-mk-repo"; "--index"]]
  | _ -> ()

let stop_file_server repo =
  match repo.repo_kind with
  | `http -> (try OpamSystem.command ["killall"; "file-server"] with _ -> ())
  | _     -> ()

let start_file_server repo =
  match repo.repo_kind with
  | `http ->
    at_exit (fun () -> stop_file_server repo);
    begin match Unix.fork () with
      | -1  -> OpamGlobals.error_and_exit "Fork error"
      | 0   ->
        let cmd = Filename.concat (Sys.getcwd ()) "file-server" in
        Unix.execvp cmd [|cmd; OpamFilename.Dir.to_string repo.repo_root|]
      | pid ->
        update_server_index repo;
        Unix.sleep 2
    end
  | _ -> ()

(* INIT *)

let init_repo_update_u kind path =
  log "init-repo-update %s\n" (OpamFilename.Dir.to_string path);
  let { repo; opam_root; contents_root } = create_config kind path in
  OpamGlobals.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo.repo_root);
  OpamRTinit.create_repo_with_history repo contents_root;
  OpamFile.Repo_config.write (OpamPath.Repository.config repo) repo;
  start_file_server repo;
  OpamGlobals.msg
    "Initializing an OPAM instance in %s/ ...\n"
    (OpamFilename.Dir.to_string opam_root);
  OPAM.init opam_root repo;
  stop_file_server repo

let init_dev_update_u contents_kind path =
  log "init-dev-update %s" (OpamFilename.Dir.to_string path);
  let { repo; opam_root; contents_root } = create_config (Some `local)  path in
  OpamGlobals.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo.repo_root);
  OpamRTinit.create_simple_repo repo contents_root contents_kind;
  OpamFile.Repo_config.write (OpamPath.Repository.config repo) repo;
  OPAM.init opam_root repo;
  OPAM.update opam_root

let init_pin_update_u contents_kind path =
  log "init-pin-update";
  init_dev_update_u contents_kind path;
  let config = read_config path in
  let pindir = config.contents_root / "a.0" in
  OpamFilename.move_dir (config.contents_root / "a.1") pindir;
  OpamGlobals.msg "Pinning a ...\n";
  OPAM.pin config.opam_root (OpamPackage.Name.of_string "a") pindir

let init_repo_update kind path =
  run (init_repo_update_u kind) path

let init_dev_update kind path =
  run (init_dev_update_u kind) path

let init_pin_update kind path =
  run (init_pin_update_u kind) path

(* TEST RUNS *)

(* Basic reposiotry update test: we verify that the global contents is
   the same as the repository contents after each new commit in the
   repository + upgrade. *)
let test_repo_update_u path =
  log "test-repo-update %s" (OpamFilename.Dir.to_string path);
  let { repo; opam_root ; _ } = read_config path in
  let commits = Git.commits repo.repo_root in
  start_file_server repo;

  (* OpamGlobals.msg "Commits:\n  %s\n\n" (String.concat "\n  " commits); *)
  List.iter (fun commit ->
      OpamGlobals.msg "%s\n" (Color.yellow "*** %s ***" commit);
      Git.checkout repo.repo_root commit;
      Git.branch repo.repo_root;
      update_server_index repo;
      OPAM.update opam_root;
      Check.packages repo opam_root;
    ) (OpamRTinit.shuffle commits);

  stop_file_server repo

(* Basic dev package update test: we install the two packages and
   update their contents *)
let test_dev_update_u path =
  log "test-base-update %s" (OpamFilename.Dir.to_string path);
  let { repo; opam_root; contents_root } = read_config path in
  let packages = OpamRepository.packages repo in
  let packages = OpamPackage.Set.fold (fun nv acc ->
      let url = read_url opam_root nv in
      match url with
      | None   -> acc
      | Some u ->
        let dir = OpamFilename.Dir.of_string (fst (OpamFile.URL.url u)) in
        if not (OpamFilename.exists_dir dir) then
          OpamGlobals.error_and_exit "Missing contents folder: %s"
            (OpamFilename.Dir.to_string dir);
        (nv, (dir, OpamRTinit.shuffle (Git.commits dir))) :: acc
    ) packages [] in

  (* install the packages *)
  List.iter (fun (nv, _) ->
      OpamGlobals.msg "Installing %s.\n" (OpamPackage.to_string nv);
      OPAM.remove opam_root (OpamPackage.name nv);
      OPAM.install opam_root (OpamPackage.name nv);
    ) packages;

  (* update and check *)
  List.iter (fun (nv, (dir, commits)) ->
      List.iter (fun commit ->
          OpamGlobals.msg "%s\n"
            (Color.yellow "%s %s" (OpamPackage.to_string nv) commit);
          Git.checkout dir commit;
          Git.branch dir;
          OPAM.update opam_root;
          OPAM.upgrade opam_root nv;
          Check.contents opam_root nv;
        ) commits
    ) packages

let test_repo_update path =
  run test_repo_update_u path

let test_dev_update path =
  run test_dev_update_u path

let test_pin_update path =
  run test_dev_update_u path

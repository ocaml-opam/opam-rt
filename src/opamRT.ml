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

let log = OpamGlobals.log "RT"

let ok () =
  OpamGlobals.msg "%s\n%!" (Color.green "[SUCCESS]")

let error e =
  OpamGlobals.msg "%s\n%s\n%!"
    (Printexc.to_string e)
    (Color.red "[ERROR]")

let newline () =
  OpamGlobals.msg "\n"

let run f x =
  try
    f x;
    newline ();
    ok ()
  with e ->
    OpamGlobals.display_messages := true;
    newline ();
    error e

let test_tag = "test"

let init_base kind path =
  log "init-base %s\n" (OpamFilename.Dir.to_string path);
  if OpamFilename.exists_dir path then
    OpamGlobals.error_and_exit "%s already exists." (OpamFilename.Dir.to_string path);
  OpamFilename.mkdir path;

  let repo_root = path / "repo" in
  OpamGlobals.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo_root);
  let commits =
    OpamRTinit.create_single_repo (OpamRepository.local repo_root) test_tag in
  List.iter (fun (pkg, commits) ->
      List.iter (fun (commit, file) ->
          OpamGlobals.msg "%s adds %s (%s)\n" commit (OpamFilename.to_string file) pkg
        ) commits
    ) commits;

  let opam_root = path / "opam" in
  OpamGlobals.msg
    "Initializing an OPAM instance in %s/ ...\n"
    (OpamFilename.Dir.to_string opam_root);
  let repo_name = OpamRepositoryName.of_string "base" in
  let repo_kind = guess_repository_kind kind repo_root in
  let repo_address = match repo_kind with
    | `git   -> OpamFilename.raw_dir
                  (Printf.sprintf "%s#%s"
                     (OpamFilename.Dir.to_string repo_root) test_tag)
    | `local -> repo_root
    | _      -> failwith "TODO" in
  let repo = {
    repo_name;
    repo_root     = OpamPath.Repository.create opam_root repo_name;
    repo_priority = 0;
    repo_address;
    repo_kind;
  } in
  OpamGlobals.display_messages := false;
  OPAM_bin.init opam_root repo;
  OpamGlobals.display_messages := true

let init_base kind path =
  run (init_base kind) path

(* First basic test: we verify that the global contents is the same as
   the repository contents after each new commit in the repository +
   upgrade. *)
let test_base path =
  log "test-base %s" (OpamFilename.Dir.to_string path);
  if not (OpamFilename.exists_dir path) then
    OpamGlobals.error_and_exit "opam-rt has not been initialized properly";
  let repo =
    let root = path / "repo" in
    OpamRepository.local root in
  let root = path / "opam" in
  let commits = Git.commits repo in
  OpamGlobals.msg "Commits:\n  %s\n\n" (String.concat "\n  " commits);
  List.iter (fun (commit) ->
      OpamGlobals.msg "%s\n" (Color.yellow "*** %s ***" commit);
      Git.checkout repo commit;
      Git.branch repo test_tag;
      OPAM_bin.update root;
      Check.packages repo root;
    ) (commits @ (List.rev commits))

let test_base path =
  run test_base path

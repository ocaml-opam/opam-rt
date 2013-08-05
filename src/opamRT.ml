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



(* INIT *)

let init_paths kind path =
  if OpamFilename.exists_dir path then
    OpamGlobals.error_and_exit "%s already exists." (OpamFilename.Dir.to_string path);
  OpamFilename.mkdir path;
  let repo_name = OpamRepositoryName.of_string "base" in
  let repo_root = path / "repo" in
  let opam_root = path / "opam" in
  let contents_root = path / "contents" in
  let repo_address = match kind with
    | Some `git   -> OpamFilename.Dir.to_string repo_root, Some Git.test_tag
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
  repo, opam_root, contents_root

let init_repo_update kind path =
  log "init-repo-update %s\n" (OpamFilename.Dir.to_string path);
  let repo, opam_root, contents_root = init_paths kind path in
  OpamGlobals.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo.repo_root);
  let commits = OpamRTinit.create_repo_with_history
      (OpamRepository.local repo.repo_root)
      contents_root in
  List.iter (fun (pkg, commits) ->
      List.iter (fun (commit, file) ->
          OpamGlobals.msg "%s adds %s (%s)\n%!"
            commit
            (OpamFilename.to_string file)
            (OpamPackage.to_string pkg)
        ) commits
    ) commits;
  OpamGlobals.msg
    "Initializing an OPAM instance in %s/ ...\n"
    (OpamFilename.Dir.to_string opam_root);
  OPAM.init opam_root repo

let init_repo_update kind path =
  run (init_repo_update kind) path

let init_dev_update contents_kind path =
  log "init-dev-update %s" (OpamFilename.Dir.to_string path);
  let repo, opam_root, contents_root = init_paths (Some `local)  path in
  OpamGlobals.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo.repo_root);
  OpamRTinit.create_simple_repo repo contents_root contents_kind;
  OPAM.init opam_root repo;
  OPAM.update opam_root

let init_dev_update kind path =
  run (init_dev_update kind) path

let shuffle l =
  let a = Array.init (List.length l) (fun _ -> None) in
  let rec aux n = function
    | []   -> ()
    | h::t ->
      let i = ref (Random.int n) in
      while a.(!i mod Array.length a) <> None do incr i done;
      a.(!i) <- Some h;
      aux (n-1) t in
  aux (List.length l) l;
  Array.fold_left (fun acc -> function
      | None   -> assert false
      | Some i -> i :: acc
    )  [] a



(* TEST RUNS *)

let repo_and_opam_root path =
  if not (OpamFilename.exists_dir path) then
    OpamGlobals.error_and_exit "opam-rt has not been initialized properly";
  let repo =
    let root = path / "repo" in
    OpamRepository.local root in
  let root = path / "opam" in
  repo, root


(* Basic reposiotry update test: we verify that the global contents is
   the same as the repository contents after each new commit in the
   repository + upgrade. *)
let test_repo_update path =
  log "test-repo-update %s" (OpamFilename.Dir.to_string path);
  let repo, root = repo_and_opam_root path in
  let commits = Git.commits repo.repo_root in
  (* OpamGlobals.msg "Commits:\n  %s\n\n" (String.concat "\n  " commits); *)
  List.iter (fun commit ->
      OpamGlobals.msg "%s\n" (Color.yellow "*** %s ***" commit);
      Git.checkout repo.repo_root commit;
      Git.branch repo.repo_root;
      OPAM.update root;
      Check.packages repo root;
    ) (shuffle commits)

let test_repo_update path =
  run test_repo_update path

(* Basic dev package update test: we install the two packages and
   update their contents *)
let test_dev_update path =
  log "test-base-update %s" (OpamFilename.Dir.to_string path);
  let repo, root = repo_and_opam_root path in
  let packages = OpamRepository.packages repo in
  let commits = OpamPackage.Set.fold (fun nv acc ->
      let dir = path / "contents" / OpamPackage.to_string nv in
      if not (OpamFilename.exists_dir dir) then
        OpamGlobals.error_and_exit "Missing contents folder: %s"
          (OpamFilename.Dir.to_string dir);
      (nv, (dir, shuffle (Git.commits dir))) :: acc
    ) packages [] in

  (* install the packages *)
  List.iter (fun (nv, _) -> OPAM.install root nv) commits;

  (* update and check *)
  List.iter (fun (nv, (dir, commits)) ->
      List.iter (fun commit ->
          OpamGlobals.msg "%s\n"
            (Color.yellow "%s %s" (OpamPackage.to_string nv) commit);
          Git.checkout dir commit;
          Git.branch dir;
          OPAM.update root;
          Check.packages repo root;
        ) commits
    ) commits

let test_dev_update path =
  run test_dev_update path

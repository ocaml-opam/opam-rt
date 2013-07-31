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

let init_base path =
  let repo_root = path / "repo" in
  let opam_root = path / "opam" in
  let repo = OpamRepository.local repo_root in
  OpamGlobals.msg
    "Creating a new repository in %s\n"
    (OpamFilename.Dir.to_string repo_root);
  let commits = OpamRTinit.create_single_repo repo in
  List.iter (fun (pkg, commit) ->
      OpamGlobals.msg "%s adds %s\n" commit pkg
    ) commits;
  OpamGlobals.msg
    "Initializing an OPAM instance in %s\n"
    (OpamFilename.Dir.to_string opam_root);
  OPAM.init repo opam_root

let check_packages_in_sync repo root =
  let packages_repo = OpamPath.Repository.packages_dir repo in
  let packages_opam = OpamPath.packages_dir root in
  let module A = OpamFilename.Attribute in
  let attributes dir =
    let all = OpamFilename.rec_files dir in
    let files = List.filter (fun f ->
        OpamFilename.ends_with "opam" f
        || OpamFilename.ends_with "url" f
        || OpamFilename.ends_with "descr" f
        (* XXX: deal with archive files *)
      ) all in
    List.fold_left (fun attrs f ->
        let attr = OpamFilename.to_attribute dir f in
        A.Set.add attr attrs
      ) A.Set.empty files in
  let attrs_repo = attributes packages_repo in
  let attrs_opam = attributes packages_opam in
  let diff1 = A.Set.diff attrs_repo attrs_opam in
  let diff2 = A.Set.diff attrs_opam attrs_repo in
  let diff = A.Set.union diff1 diff2 in
  if not (A.Set.is_empty diff) then (
    A.Set.iter (fun a ->
        OpamGlobals.msg "error: %s\n" (A.to_string a)
      ) diff;
    failwith "package metadata not in sync"
  )

(* First basic test: we verify that the global contents is the same as
   the repository contents after each new commit in the repository +
   upgrade. *)
let test_base path =
  let repo =
    let root = path / "repo" in
    OpamRepository.local root in
  let root = path / "opam" in
  let commits = Git.commits repo in
  List.iter (fun (commit) ->
      Git.checkout repo commit;
      OPAM.update root;
      check_packages_in_sync repo root
    ) commits;
  OpamGlobals.msg "test_update_base: SUCCESS!\n"

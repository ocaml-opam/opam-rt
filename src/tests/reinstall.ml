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
open OpamFilename.Op

let name = "reinstall"

let init_u contents_kind path =
  log "init-pin-install";
  let { repo_name; repo_root; repo_url; opam_root; contents_root } =
    create_config (Some `rsync)  path
  in
  OpamConsole.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo_root);
  create_simple_repo repo_root contents_root contents_kind;
  let packages =
    let a = package "a" 1 (Some `rsync) contents_root 450 in
    let b =
      Packages.add_depend_with_runtime_checks opam_root
        (package "b" 1 (Some `rsync) contents_root 451)
        "a"
    in
    let c =
      Packages.add_depend_with_runtime_checks opam_root
        (package "c" 1 (Some `rsync) contents_root 452)
        "b"
    in
    let d =
      Packages.add_depend_with_runtime_checks opam_root
        (package "d" 1 (Some `rsync) contents_root 453)
        "c"
    in
    [ a; b; c; d ]
  in
  List.iter (Packages.add repo_root contents_root) packages;
  write_repo_config path repo_name (repo_url, None);
  Opamlib.init opam_root repo_name repo_url;
  Opamlib.update opam_root

let init kind = check_and_run kind (init_u kind)


let test_u path =
  let { repo_root; opam_root; contents_root; _ } = read_config path in
  let a = OpamPackage.Name.of_string "a" in
  let b = OpamPackage.Name.of_string "b" in
  let c = OpamPackage.Name.of_string "c" in
  let d = OpamPackage.Name.of_string "d" in
  let v version = OpamPackage.Version.of_string (string_of_int version) in
  let (-) = OpamPackage.create in
  let pkg name = name - v 1 in
  let step = step () in
  step "Install d";
  Opamlib.install opam_root d;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Reinstall b";
  Opamlib.reinstall opam_root b;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Install d";
  Opamlib.install opam_root d;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Remove b";
  Opamlib.remove opam_root b;
  check_installed path ~roots:[] (List.map pkg [a]);
  step "Install d";
  Opamlib.install opam_root d;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Remove b from upstream and update";
  OpamFilename.rmdir (path / "repo" / "packages" / "b.1");
  Opamlib.update opam_root;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Upgrade";
  Opamlib.upgrade opam_root [];
  check_installed path ~roots:[] (List.map pkg [a]);
  step "Attempt to reinstall d";
  should_fail (Opamlib.install_code opam_root d) `No_solution;
  step "Revert to the state with all packages installed and b removed upstream";
  let b1 =
    Packages.add_depend_with_runtime_checks opam_root
      (package "b" 1 (Some `rsync) contents_root 451)
      "a"
  in
  Packages.add repo_root contents_root b1;
  Opamlib.update opam_root;
  Opamlib.install opam_root d;
  OpamFilename.rmdir (path / "repo" / "packages" / "b.1");
  Opamlib.update opam_root;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Reinstall c";
  Opamlib.reinstall opam_root c;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Add a new version of c, then upgrade";
  let c2 =
    Packages.add_depend_with_runtime_checks opam_root
      (package "c" 2 (Some `rsync) contents_root 552)
      "b"
  in
  Packages.add repo_root contents_root c2;
  Opamlib.update opam_root;
  Opamlib.upgrade opam_root [c];
  check_installed path ~roots:[pkg d] [a-v 1; b-v 1; c-v 2; d-v 1];
  step "Try to reinstall b (should work using the cache)";
  Opamlib.reinstall opam_root b;
  check_installed path ~roots:[pkg d] [a-v 1; b-v 1; c-v 2; d-v 1];
  step "Try to reinstall a";
  Opamlib.reinstall opam_root a;
  check_installed path ~roots:[pkg d] [a-v 1; b-v 1; c-v 2; d-v 1];
  step "Add a new version of a and upgrade";
  let a2 = package "a" 2 (Some `rsync) contents_root 452 in
  Packages.add repo_root contents_root a2;
  Opamlib.update opam_root;
  Opamlib.upgrade opam_root [];
  check_installed path ~roots:[] [a-v 2];
  step "Remove that new version of a and upgrade";
  OpamFilename.rmdir (path / "repo" / "packages" / "prefix-a" / "a.2");
  Opamlib.update opam_root;
  Opamlib.upgrade opam_root [];
  check_installed path ~roots:[] [a-v 1];
  step "Upgrade again";
  Opamlib.upgrade opam_root [];
  check_installed path ~roots:[] [a-v 1];
  ()

let run kind = check_and_run kind test_u

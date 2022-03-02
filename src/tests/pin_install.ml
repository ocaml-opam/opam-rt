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
open OpamTypes

let name = "pin-install"

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
    let a1 = package "a" 1 (Some `rsync) contents_root 442 in
    let a2 = package "a" 2 (Some `rsync) contents_root 443 in
    let b1 = package "b" 1 (Some `rsync) contents_root 444 in
    let b2 = package "b" 2 (Some `rsync) contents_root 445 in
    let b1 = Packages.add_depend b1 "a" ~formula:(Atom (`Eq, FString "1")) in
    let b2 = Packages.add_depend b2 "a" ~formula:(Atom (`Eq, FString "2")) in
    [ a1; a2; b1; b2 ]
  in
  List.iter (Packages.add repo_root contents_root) packages;
  write_repo_config path repo_name (repo_url, None);
  Opamlib.init opam_root repo_name repo_url;
  Opamlib.update opam_root;
  let config = read_config path in
  let pindir = config.contents_root / "a.pinned" in
  OpamFilename.copy_dir ~src:(config.contents_root / "a.1") ~dst:pindir;
  OpamConsole.msg "Pinning a ...\n";
  Opamlib.pin config.opam_root (OpamPackage.Name.of_string "a") pindir

let init kind = check_and_run kind (init_u kind)


let test_u path =
  log "test-pin-update %s" (OpamFilename.Dir.to_string path);
  let { repo_root; opam_root; contents_root; _ } =
    read_config path
  in
  let b = OpamPackage.Name.of_string "b" in
  let a = OpamPackage.Name.of_string "a" in
  let v version = OpamPackage.Version.of_string (string_of_int version)
  in
  let (-) = OpamPackage.create in
  let overlay name =
    OpamPath.Switch.Overlay.opam opam_root Opamlib.default_switch name
  in
  let map_overlay f pkg =
    let o = overlay pkg in
    OpamFile.OPAM.write o (f (OpamFile.OPAM.read o))
  in
  let step = step () in
  step "Install b (version 2)";
  Opamlib.install opam_root b;
  check_installed path ~roots:[ b-v 2 ] [ b-v 2 ];
  step "Attempt to install b.1 (should fail because a is pinned to 2)";
  should_fail (Opamlib.install_code opam_root b ~version:(v 1)) `No_solution;
  check_installed path ~roots:[ b-v 2 ] [ a-v 2;];
  step "Cleanup";
  Opamlib.remove opam_root ~auto:true b;
  check_installed path [];
  step "Change pinned version of a to 1";
  map_overlay (OpamFile.OPAM.with_version (v 2)) a;
  step "Attempt to install b 2";
  should_fail (Opamlib.install_code opam_root b ~version:(v 4)) `No_solution;
  check_installed path [];
  step "Install b, should get version 1";
  Opamlib.install opam_root b;
  check_installed path ~roots:[ b-v 1 ] [ b-v 1; a-v 1 ];
  step "Change pinned version of installed package a back to 2";
  map_overlay (OpamFile.OPAM.with_version (v 4)) a;
  Opamlib.upgrade opam_root [];
  check_installed path ~roots:[ b-v 2 ] [ b-v 2; a-v 2 ];
  (* -- *)
  step "Remove all, unpin a, add a new version of a and update";
  Opamlib.remove opam_root a;
  Opamlib.unpin opam_root a;
  let a3 = package "a" 3 (Some `rsync) contents_root 452 in
  Packages.add repo_root contents_root a3;
  Opamlib.update opam_root;
  check_installed path [];
  step "Pin a to version 2 and install";
  Opamlib.vpin opam_root a (v 2);
  Opamlib.install opam_root a;
  check_installed path ~roots:[a-v 2] [a-v 2];
  step "Unpin a";
  Opamlib.unpin opam_root a;
  check_installed path ~roots:[a-v 2] [a-v 2];
  step "Upgrade";
  Opamlib.upgrade opam_root [];
  check_installed path ~roots:[a-v 3] [a-v 3]

let run  kind = check_and_run kind test_u

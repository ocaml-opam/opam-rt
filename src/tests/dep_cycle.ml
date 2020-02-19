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
open OpamTypes

let name = "dep-cycle"

let init_u kind path =
  log "init-dep-cycle";
  let { repo_name; repo_root; repo_url; opam_root; contents_root } =
    create_config (Some `rsync) path
  in
  OpamConsole.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo_root);
  create_simple_repo repo_root contents_root kind;
  let packages =
    let a1 = package "a" 1 (Some `rsync) contents_root 450 in
    let a2 = package "a" 2 (Some `rsync) contents_root 451 in
    let b1 = package "b" 1 (Some `rsync) contents_root 452 in
    let b2 = package "b" 2 (Some `rsync) contents_root 453 in
    let formula =
      Atom (`Eq,
            FIdent (OpamFilter.ident_of_var
                      (OpamVariable.Full.of_string "version")))
    in
    let a1 =
      Packages.add_depend_with_runtime_checks opam_root a1 ~formula "b"
    in
    let b2 =
      Packages.add_depend_with_runtime_checks opam_root b2 ~formula "a"
    in
    [ a1; a2; b1; b2 ]
  in
  List.iter (Packages.add repo_root contents_root) packages;
  write_repo_config path repo_name (repo_url, None);
  Opamlib.init opam_root repo_name repo_url;
  Opamlib.update opam_root

let init kind = check_and_run kind (init_u kind)


let run_u path =
  let { opam_root; _ } = read_config path in
  let a = OpamPackage.Name.of_string "a" in
  let b = OpamPackage.Name.of_string "b" in
  let v version = OpamPackage.Version.of_string (string_of_int version) in
  let (-) = OpamPackage.create in
  let step = step () in
  step "Install a1";
  Opamlib.install opam_root a ~version:(v 1);
  check_installed path [a-v 1;b-v 1];
  step "Upgrade";
  Opamlib.upgrade opam_root [];
  check_installed path [a-v 2;b-v 2];
  step "Downgrade a";
  Opamlib.install opam_root a ~version:(v 1);
  check_installed path [a-v 1;b-v 1];
  step "Upgrade";
  Opamlib.upgrade opam_root [];
  check_installed path [a-v 2;b-v 2];
  step "Remove b then downgrade a";
  Opamlib.remove opam_root b;
  Opamlib.install opam_root a ~version:(v 1);
  check_installed path [a-v 1;b-v 1]

let run kind = check_and_run kind run_u

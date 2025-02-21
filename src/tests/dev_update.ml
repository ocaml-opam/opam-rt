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

let name = "dev-update"

let init_u contents_kind path =
  log "init-dev-update %s" (OpamFilename.Dir.to_string path);
  let { repo_name; repo_root; repo_url ; opam_root; contents_root } =
    create_config (Some `rsync)  path
  in
  OpamConsole.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo_root);
  create_simple_repo repo_root contents_root contents_kind;
  write_repo_config path repo_name repo_url None;
  Opamlib.init opam_root repo_name repo_url;
  Opamlib.update opam_root

let init kind = check_and_run kind (init_u kind)


(* Basic dev package update test: we install the two packages and
   update their contents *)
let test_u path =
  log "test-base-update %s" (OpamFilename.Dir.to_string path);
  let { repo_root; opam_root; _ } =
    read_config path
  in
  let opams = Opamlib.repo_opams repo_root in
  let opams =
    OpamPackage.Map.union (fun _ x -> x) opams @@
    Opamlib.repo_opams
      (OpamPath.Switch.Overlay.dir opam_root Opamlib.default_switch)
  in
  let packages =
    OpamPackage.Map.fold (fun nv opam acc ->
        let url = OpamFile.OPAM.url opam in
        match url with
        | None   -> acc
        | Some u ->
          match OpamUrl.local_dir (OpamFile.URL.url u) with
          | Some dir ->
            (nv, (dir, shuffle (Git.commits dir))) :: acc
          | None ->
            acc
      ) opams []
  in

  (* install the packages *)
  List.iter (fun (nv, _) ->
      OpamConsole.msg "Installing %s.\n" (OpamPackage.to_string nv);
      Opamlib.remove opam_root (OpamPackage.name nv);
      Opamlib.install opam_root (OpamPackage.name nv);
    ) packages;

  (* update and check *)
  List.iter (fun (nv, (dir, commits)) ->
      List.iter (fun commit ->
          OpamConsole.msg "%s\n"
            (OpamConsole.colorise `yellow @@
             Printf.sprintf "%s %s" (OpamPackage.to_string nv) commit);
          Git.checkout dir commit;
          Git.branch dir;
          Opamlib.update opam_root;
          Opamlib.upgrade opam_root [];
          Check.contents opam_root nv (OpamPackage.Map.find nv opams);
        ) commits
    ) packages

let run kind = check_and_run kind test_u

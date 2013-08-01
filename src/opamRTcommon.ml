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

open OpamTypes

module Color = struct

  let red fmt =
    Printf.ksprintf (fun s -> Printf.sprintf "\027[31m%s\027[m" s) fmt

  let green fmt =
    Printf.ksprintf (fun s -> Printf.sprintf "\027[32m%s\027[m" s) fmt

  let yellow fmt =
    Printf.ksprintf (fun s -> Printf.sprintf "\027[33m%s\027[m" s) fmt

end

module Git = struct

  let exec repo command =
    OpamFilename.in_dir repo.repo_root (fun () ->
        OpamSystem.command command
      )

  let return_one_line repo command =
    OpamFilename.in_dir repo.repo_root (fun () ->
        List.hd (OpamSystem.read_command_output command)
      )

  let return repo command =
    OpamFilename.in_dir repo.repo_root (fun () ->
        (OpamSystem.read_command_output command)
      )

  let commit repo msg =
    exec repo [ "git"; "commit"; "-a"; "-m"; msg; "--allow-empty" ]

  let revision repo =
    return_one_line repo [ "git"; "rev-parse"; "HEAD" ]

  let commits repo =
    return repo ["git"; "log"; "master"; "--pretty=format:%H"]

  let init repo =
    exec repo ["git"; "init"]

  let branch repo tag =
    exec repo ["git"; "checkout"; "-B"; tag]

  let add repo file =
    if OpamFilename.exists file then
      let file = OpamFilename.remove_prefix repo.repo_root file in
      exec repo ["git"; "add"; file]

  let checkout repo hash =
    exec repo ["git"; "checkout"; hash];
    exec repo ["git"; "clean"; "-fdx"]

end

module Packages = struct

  open OpamFile

  type t = {
    pkg    : string;
    prefix : string option;
    opam   : OPAM.t;
    url    : URL.t option;
    descr  : Descr.t option;
    archive: string option;
  }

  let opam package seed =
    let opam = OPAM.create (OpamPackage.of_string package) in
    let maintainer = "test-" ^ string_of_int seed in
    OPAM.with_maintainer opam maintainer

  let url seed =
    let url = URL.empty in
    let checksum = "test-" ^ string_of_int seed in
    URL.with_checksum url checksum

  let descr seed =
    Descr.of_string ("Test " ^ seed)

  let files repo prefix nv =
    let opam = OpamPath.Repository.opam repo prefix nv in
    let descr = OpamPath.Repository.descr repo prefix nv in
    let url = OpamPath.Repository.url repo prefix nv in
    let archive = OpamPath.Repository.archive repo nv in
    opam, descr, url, archive

  let files_of_t repo t =
    files repo t.prefix (OpamPackage.of_string t.pkg)

  let write_o f = function
    | None   -> ()
    | Some x -> f x

  let write repo t =
    let opam, descr, url, archive = files_of_t repo t in
    OPAM.write opam t.opam;
    write_o (Descr.write descr) t.descr;
    write_o (URL.write url) t.url;
    write_o (OpamFilename.write archive) t.archive

  let read_o f file =
    if OpamFilename.exists file then Some (f file)
    else None

  let read repo prefix nv =
    let opam, descr, url, archive = files repo prefix nv in
    let opam = OPAM.read opam in
    let descr = read_o Descr.read descr in
    let url = read_o URL.read url in
    let archive = read_o OpamFilename.read archive in
    let pkg = OpamPackage.to_string nv in
    { pkg; prefix; opam; descr; url; archive }

  let add repo t =
    write repo t;
    let opam, descr, url, archive = files_of_t repo t in
    let commit file =
      if OpamFilename.exists file then (
        Git.add repo file;
        let msg =
          Printf.sprintf "Add package %s (%s)" t.pkg (OpamFilename.to_string file) in
        Git.commit repo msg;
        Some (Git.revision repo, file);
      ) else
        None in
    let commits = OpamMisc.filter_map commit [opam; descr; url; archive] in
    (t.pkg, commits)

end

module OPAM_lib = struct

  let init opam_root repo =
    OpamGlobals.root_dir := OpamFilename.Dir.to_string opam_root;
    OpamClient.API.init repo OpamCompiler.system
      ~jobs:1 `sh (OpamFilename.raw "dummy") `no

  let update path =
    OpamGlobals.root_dir := OpamFilename.Dir.to_string path;
    OpamClient.API.update []

end

module OPAM_bin = struct

  let opam opam_root command args =
    let debug = if !OpamGlobals.debug then ["--debug"] else [] in
    OpamSystem.command
      ("opam" :: command ::
         ["--root"; (OpamFilename.Dir.to_string opam_root)]
         @ debug
         @ args)

  let init opam_root repo =
    let kind = string_of_repository_kind repo.repo_kind in
    opam opam_root "init" [
      OpamRepositoryName.to_string repo.repo_name;
      OpamFilename.Dir.to_string repo.repo_address;
      "--no-setup"; "--no-base-packages";
      "--kind"; kind
    ]

  let update opam_root =
    opam opam_root "update" []

end

module Check = struct

  exception Sync_error of (string * file_attribute * filename) list

  let packages repo root =
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
          let root = OpamFilename.dirname_dir (OpamFilename.dirname f) in
          let attr = OpamFilename.to_attribute root f in
          A.Map.add attr f attrs
        ) A.Map.empty files in

    let attrs_repo = attributes packages_repo in
    let attrs_opam = attributes packages_opam in

    let set map = A.Map.fold (fun a _ set -> A.Set.add a set) map A.Set.empty in
    let attrs_repo_s = set attrs_repo in
    let attrs_opam_s = set attrs_opam in

    let diff1 = A.Set.diff attrs_repo_s attrs_opam_s in
    let diff2 = A.Set.diff attrs_opam_s attrs_repo_s in
    let diff = A.Set.union diff1 diff2 in

    if not (A.Set.is_empty diff) then (
      let errors = A.Set.fold (fun a errors ->
          let source, attr, file =
            if A.Map.mem a attrs_repo then
              ("repository", a, A.Map.find a attrs_repo)
            else
              ("opam", a, A.Map.find a attrs_opam) in
          (source, attr, file) :: errors
        ) diff [] in
      OpamGlobals.error "\n%s" (Color.red " -- Sync error --");
      List.iter (fun (source, attr, file) ->
          OpamGlobals.error "%s: %s\n%s\n%s"
            source
            (A.to_string attr) (OpamFilename.to_string file) (OpamFilename.read file)
        ) errors;
      raise (Sync_error errors)
    )

end

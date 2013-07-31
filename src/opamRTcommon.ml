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
    exec repo [ "git"; "commit"; "-a"; "-m"; msg ]

  let revision repo =
    return_one_line repo [ "git"; "rev-parse"; "HEAD" ]

  let commits repo =
    return repo ["git"; "log"; "--pretty=format:%%H"]

  let init repo =
    exec repo ["git"; "init"]

  let add repo file =
    if OpamFilename.exists file then
      let file = OpamFilename.remove_prefix repo.repo_root file in
      exec repo ["git"; "add"; file]

  let checkout repo hash =
    exec repo ["git"; "checkout"; hash]

end

module Packages = struct

  open OpamFile

  type t = {
    pkg   : string;
    prefix: string option;
    opam  : OPAM.t;
    url   : URL.t option;
    descr : string option;
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
    "Test " ^ seed

  let files repo t =
    let nv = OpamPackage.of_string t.pkg in
    let opam = OpamPath.Repository.opam repo t.prefix nv in
    let descr = OpamPath.Repository.descr repo t.prefix nv in
    let url = OpamPath.Repository.url repo t.prefix nv in
    opam, descr, url

  let write repo t =
    let opam, descr, url = files repo t in
    OPAM.write opam t.opam;
    begin match t.url with
      | None   -> ()
      | Some u -> URL.write url u
    end;
    match t.descr with
    | None   -> ()
    | Some d -> OpamFilename.write descr d

  let read repo prefix nv =
    let opam = OpamPath.Repository.opam repo prefix nv in
    let descr = OpamPath.Repository.opam repo prefix nv in
    let url = OpamPath.Repository.opam repo prefix nv in
    let opam = OPAM.read opam in
    let descr =
      if OpamFilename.exists descr then Some (OpamFilename.read descr) else None in
    let url = if OpamFilename.exists url then Some (URL.read url) else None in
    let pkg = OpamPackage.to_string nv in
    { pkg; prefix; opam; descr; url }

  let add repo t =
    write repo t;
    let opam, descr, url = files repo t in
    List.iter (Git.add repo) [opam; descr; url];
    Git.commit repo ("Add package " ^ t.pkg);
    (t.pkg, Git.revision repo)

end

module OPAM = struct

  let init repo path =
    OpamGlobals.root_dir := OpamFilename.Dir.to_string path;
    OpamClient.API.init repo OpamCompiler.system
      ~jobs:1 `sh (OpamFilename.raw "dummy") `no

  let update path =
    OpamGlobals.root_dir := OpamFilename.Dir.to_string path;
    OpamClient.API.update []

end

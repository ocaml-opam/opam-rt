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

open OpamRTcommon
open OpamTypes
open OpamFilename.OP

let package name version kind root seed =
  let pkg = Printf.sprintf "%s.%d" name version in
  let nv = OpamPackage.of_string pkg in
  let contents = Contents.create nv in
  Packages.({
    nv;
    prefix   = prefix nv;
    opam     = opam nv seed;
    url      = url kind (root / pkg) seed;
    descr    = descr seed;
    contents;
    archive  = archive contents nv seed;
  })

let a1 root =
  package "a" 1 (Some `local) root

let a2 root =
  package "a" 2 (Some `git) root

let not_very_random n =
  let i = Random.int n in
  if i > Pervasives.(/) n 2 then 0 else i

let ar root _ =
  let seed = not_very_random 10 in
  if Random.int 2 = 0 then
    a1 root seed
  else
    a2 root seed

let all root = [
  a1 root 0;
  a1 root 1;
  a1 root 2;
  a2 root 2;
  a2 root 1;
  a2 root 0;
] @ Array.to_list (Array.init 10 (ar root))

let create_single_repo repo tag =
  OpamFilename.mkdir repo.repo_root;
  Git.init repo.repo_root;
  let all = all repo.repo_root in
  let commits = List.map (Packages.add repo) all in
  Git.branch repo.repo_root tag;
  commits

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

let package name version seed =
  let pkg = Printf.sprintf "%s.%d" name version in
  Packages.({
    pkg     = pkg;
    prefix  = prefix name version;
    opam    = opam pkg seed;
    url     = url seed;
    descr   = descr seed;
    archive = archive seed;
  })

let a1 = package "a" 1
let a2 = package "a" 2

let not_very_random n =
  let i = Random.int n in
  if i > n/2 then 0 else i

let ar _ =
  package "a" (Random.int 10) (not_very_random 10)

let all = [
  a1 0;
  a1 1;
  a1 2;
  a2 2;
  a2 1;
  a2 0;
] @ Array.to_list (Array.init 10 ar)

let create_single_repo repo tag =
  OpamFilename.mkdir repo.repo_root;
  Git.init repo;
  let commits = List.map (Packages.add repo) all in
  Git.branch repo tag;
  commits

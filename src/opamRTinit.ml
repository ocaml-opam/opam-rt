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

let a1 seed = Packages.({
    pkg    = "a.1";
    prefix = None;
    opam   = opam "a.1" seed;
    url    = None;
    descr  = None;
  })

let a2 seed = Packages.({
    pkg    = "a.2";
    prefix = None;
    opam   = opam "a.2" seed;
    url    = None;
    descr  = None;
  })

let all = [
  a1 0;
  a1 1;
  a2 0;
  a2 1;
]

let create_single_repo repo =
  Git.init repo;
  List.map (Packages.add repo) all

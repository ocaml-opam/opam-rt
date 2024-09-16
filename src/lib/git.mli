(*
 * Copyright (c) 2013-2019 OCamlPro
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

open OpamTypes

val commit: dirname -> ('a, unit, string, unit) format4 -> 'a

val commit_file:
  dirname -> filename -> ('a, unit, string, unit) format4 -> 'a

val commit_dir:
  dirname -> dirname -> ('a, unit, string, unit) format4 -> 'a

val commit_files_and_dirs:
  dirname -> filename list -> dirname list -> ('a, unit, string, unit) format4 -> 'a

val revision: dirname -> string

val commits: dirname -> string list

val init: dirname -> unit

val test_tag: string

val branch: dirname -> unit

val master: dirname -> unit

val add: dirname -> filename -> unit

val add_dir: dirname -> dirname -> unit

val add_list: dirname -> filename list -> unit

val checkout: dirname -> string -> unit

val msg:
  dirname -> string -> package -> ('a, unit, string, unit) format4 -> 'a



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

type t = {
  nv: package;
  prefix: string option;
  opam: OpamFile.OPAM.t;
  files: (basename * string * int) list;
  contents: (basename * string * int) list;
  archive: string option;
}

val opam:
  package -> [> `git | `rsync ] option -> dirname -> int -> OpamFile.OPAM.t

val mandatory_fields: string -> ?seed:int -> OpamFile.OPAM.t -> OpamFile.OPAM.t

val add_depend:
  t -> ?formula:(relop * filter) generic_formula -> string -> t

val add_depend_with_runtime_checks:
  dirname -> t -> ?formula:(relop * filter) generic_formula -> string -> t

val archive:
  (basename * string * int) list -> package -> int -> string option

val prefix: package -> string option

val files: int -> (basename * string * int) list

val write: dirname -> dirname -> t -> unit

val read: dirname -> dirname -> string option -> package -> t

val add: dirname -> dirname -> t -> unit

(** Create package content *)

val content_create: package -> int -> (basename * string * int) list

val content_read: dirname -> package -> (basename * string * int) list

val content_write: dirname -> package -> (basename * string * int) list -> unit


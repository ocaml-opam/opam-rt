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

val default_switch: switch

(** opam-lib functions *)

val repo_opams: dirname -> OpamFile.OPAM.t package_map


(** CLI functions *)

val opam:
  ?fake:bool ->
  ?env:string list -> dirname -> string -> string list -> unit

val var: dirname -> string -> string

val init: dirname -> repository_name -> url -> unit

val install: dirname -> ?version:version -> name -> unit

val install_code: dirname -> ?version:version -> name -> int

val install_dir: dirname -> ?recs:bool -> ?subpath:string -> dirname -> unit

val reinstall: dirname -> ?version:version -> name -> unit

val remove: dirname -> ?auto:bool -> name -> unit

val remove_dir:
  dirname -> ?auto:bool -> ?recs:bool -> ?subpath:string -> dirname -> unit

val update: dirname -> unit

val upgrade: dirname -> ?fake:bool -> name list -> unit

val pin:
  dirname -> ?recs:bool -> ?subpath:string -> ?action:bool
  -> name -> dirname -> unit

val vpin: dirname -> name -> version -> unit

val pin_kind: dirname -> ?action:bool -> ?kind:string -> name -> string -> unit

val pin_dir:
  dirname -> ?recs:bool -> ?subpath:string -> ?action:bool -> ?kind:string
  -> dirname -> unit

val unpin: dirname -> ?action:bool -> name -> unit

val unpin_dir:
  dirname -> ?recs:bool -> ?subpath:string -> ?action:bool -> dirname -> unit

val pin_edit: dirname -> ?action:bool -> name -> (filename -> 'a) -> unit

val pinned: dirname -> string list list

val import: dirname -> ?fake:bool -> filename -> unit

val export: dirname -> filename -> unit


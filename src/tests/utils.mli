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
open OpamTypes

exception Not_available
exception Allowed_failure

val log: ('a, Format.formatter, unit) format -> 'a

val set_seed: int -> unit

val set_datadir: dirname -> unit

val data: string -> filename

(** Config *)

type config = {
  repo_name    : OpamRepositoryName.t;
  repo_root    : dirname;
  repo_url     : url;
  opam_root    : dirname;
  contents_root: dirname;
}

val create_config: OpamUrl.backend option -> dirname -> config

val read_config: dirname -> config

val write_repo_config:
  dirname -> repository_name -> OpamUrl.t -> trust_anchors option -> unit

(** Init *)
val shuffle: 'a list -> 'a list

val package:
  string -> int -> OpamUrl.backend option -> dirname -> ?gener_archive:bool ->
  int -> Packages.t

(* Repository initialisation *)
val create_repo_with_history:
  dirname -> dirname -> unit

val create_simple_repo:
  dirname -> dirname -> OpamUrl.backend option -> unit

(** opam *)

type installed = { installed: package_set; installed_roots: package_set }

val read_installed:
  dirname -> installed

val check_installed:
  dirname -> ?roots:package list -> package list -> unit

val check_pinned:
  dirname ->
  ?kind:string -> package list -> unit

(* Repo server *)
val update_server_index:
  dirname -> OpamUrl.t -> unit

val start_file_server:
  dirname -> OpamUrl.t -> unit -> unit

(** Run *)
val run: ('a -> unit) -> 'a -> unit

val check_and_run:
  OpamUrl.backend option -> ('a -> unit) -> 'a -> unit

val step: unit -> string -> unit

val should_fail:
  int -> OpamStd.Sys.exit_reason -> unit

val todo: unit -> 'a

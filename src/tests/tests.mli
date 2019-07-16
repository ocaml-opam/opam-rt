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


(* Set of tests *)
open OpamTypes

val set_seed: int -> unit
val set_datadir: dirname -> unit

exception Not_available
exception Allowed_failure

module type TEST = sig
  val name: string
  val init: OpamUrl.backend option -> dirname -> unit
  val run: OpamUrl.backend option -> dirname -> unit
end

(** Defined tests :
    * repo_update
    * dev_update
    * pin_update
    * pin_install
    * reinstall
    * pin_advanced
    * dep_cycle
    * big_upgrade
*)
val tests: (string * (module TEST)) list

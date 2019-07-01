(*
 * Copyright (c) 2013-2019 OCamlPro
 * Authors Thomas Gazagnaire <thomas@gazagnaire.org>,
 *         Louis Gesbert <louis.gesbert@ocamlpro.com>
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

val exit : OpamStd.Sys.exit_reason -> 'a
val log : ('a, out_channel, unit) format -> 'a
val ok : unit -> unit
val error : exn -> 'a
val newline : unit -> unit
val run : ('a -> 'b) -> 'a -> unit
type config = {
  repo_name : repository_name;
  repo_root : dirname;
  repo_url : url;
  opam_root : dirname;
  contents_root : dirname;
}
val base_repo_name : repository_name
val create_config : [> `git | `http | `rsync ] option -> dirname -> config
val repos_config_file : dirname -> 'a OpamFile.typed_file
val read_config : dirname -> config
val write_repo_config :
  dirname -> repository_name -> url * trust_anchors option -> unit
type installed = { installed : package_set; installed_roots : package_set; }
val read_installed : dirname -> installed
val check_installed : dirname -> ?roots:package list -> package list -> unit
val update_server_index : dirname -> url -> unit
val start_file_server : dirname -> url -> unit -> unit
val init_repo_update_u : [> `git | `http | `rsync ] option -> dirname -> unit
val init_dev_update_u : [> `git | `rsync ] option -> dirname -> unit
val init_pin_update_u : [> `git | `rsync ] option -> dirname -> unit
val init_pin_install_u : [> `git | `rsync ] option -> dirname -> unit
val init_reinstall_u : [> `git | `rsync ] option -> dirname -> unit
val test_repo_update_u : dirname -> unit
val test_dev_update_u : dirname -> unit
val test_pin_install_u : dirname -> unit
val test_reinstall_u : dirname -> unit
val todo : unit -> 'a
val check_and_run : [> `http ] option -> ('a -> 'b) -> 'a -> unit

module type TEST = sig
  val name : string
  val init : OpamUrl.backend option -> dirname -> unit
  val run : OpamUrl.backend option -> dirname -> unit
end
module Repo_update : TEST
module Dev_update : TEST
module Pin_update : TEST
module Pin_install : TEST
module Reinstall : TEST
module Dep_cycle : TEST
module Pin_advanced : TEST
module Big_upgrade : TEST
val tests : (string * (module TEST)) list

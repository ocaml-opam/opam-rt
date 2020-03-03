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

open Utils
open OpamFilename.Op

let name = "pin-update"

let init_u contents_kind path =
  log "init-pin-update";
  Dev_update.init_u contents_kind path;
  let config = read_config path in
  let pindir = config.contents_root / "a.0" in
  OpamFilename.move_dir ~src:(config.contents_root / "a.1") ~dst:pindir;
  OpamConsole.msg "Pinning a ...\n";
  Opamlib.pin config.opam_root (OpamPackage.Name.of_string "a") pindir

let init kind = check_and_run kind (init_u kind)


let run  kind = check_and_run kind Dev_update.test_u

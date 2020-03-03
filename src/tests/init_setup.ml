(*
 * Copyright (c) 2020 OCamlPro
 * Authors Raja Boujbel <raja.boujbel@ocamlpro.com>
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

let name = "init-setup"

let local kind stuff =
  if kind <> Some `rsync then
    (OpamConsole.msg "%s\n" (OpamConsole.colorise `yellow "[SKIP]");
     raise Not_available)
  else stuff ()

let init kind path =
  local kind @@ fun () ->
  log "init-init-setup %s\n" (OpamFilename.Dir.to_string path);
  let { repo_name; repo_root; repo_url; opam_root; contents_root } =
    create_config (Some `rsync)  path
  in
  OpamConsole.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo_root);
  create_simple_repo repo_root contents_root kind;
  write_repo_config path repo_name (repo_url, None);
  OpamConsole.msg "Initializing an opam instance in %s/ ...\n"
    (OpamFilename.Dir.to_string opam_root);
  OpamClientConfig.update ();
  Opamlib.opam opam_root "init" [
    OpamRepositoryName.to_string repo_name;
    OpamUrl.to_string repo_url;
    "--no-setup"; "--bare"
  ]

let present files =
  let present, absent = List.partition OpamFilename.exists files in
  if present <> [] then
    OpamConsole.msg "%s present: %s\n"
      (OpamConsole.colorise `green "[OK]")
      (OpamStd.Format.pretty_list (List.map OpamFilename.to_string present));
  if absent <> [] then
    OpamConsole.msg "%s %s missing\n"
      (OpamConsole.colorise `red "[KO]")
      (OpamStd.Format.pretty_list (List.map OpamFilename.to_string absent))

let absent files =
  let present, absent = List.partition OpamFilename.exists files in
  if absent <> [] then
    OpamConsole.msg "%s %s absent\n"
      (OpamConsole.colorise `green "[OK]")
      (OpamStd.Format.pretty_list (List.map OpamFilename.to_string absent));
  if present <> [] then
    OpamConsole.msg "%s %s present\n"
      (OpamConsole.colorise `red "[KO]")
      (OpamStd.Format.pretty_list (List.map OpamFilename.to_string present))

let run kind path =
  local kind @@ fun () ->
  let { opam_root; _ } = read_config path in
  let setitup ?hook ?complete arg =
    let args =
      (match hook with
       | None -> []
       | Some false -> ["--disable-shell-hook"]
       | Some true -> ["--enable-shell-hook"])
      @
      (match complete with
       | None -> []
       | Some false -> ["--disable-completion"]
       | Some true -> ["--enable-completion"])
      @ arg
    in
    Opamlib.opam opam_root "init" args
  in
  (* files *)
  let opam_init = opam_root / "opam-init" in
  let init = opam_init // "init.sh" in
  let complete = opam_init // "complete.sh" in
  let env_hook = opam_init // "env_hook.sh" in
  let sandbox = opam_init / "hooks" // "sandbox.sh" in
  (* tests *)
  let step = step () in
  step "Check initial setup";
  present [init; complete; sandbox];
  absent [env_hook];
  step "Enable shell hook";
  setitup ~hook:true [];
  present [init; sandbox; complete; env_hook];
  step "Disable completion";
  setitup ~complete:false [];
  present [init; sandbox; env_hook];
  absent [complete];
  step "Auto full setitup";
  setitup ["-a"];
  present [init; sandbox; complete; env_hook];
  step "No setup interactive";
  setitup ["-ni"];
  present [init; sandbox; complete; env_hook];
  step "No setup interactive, no completion";
  setitup ~complete:false [];
  setitup ["-ni"];
  present [init; sandbox; env_hook];
  absent [complete];
  step "No setup interactive, no env hook";
  setitup ~hook:false [];
  setitup ["-ni"];
  present [init; sandbox];
  absent [complete; env_hook];
  step "Enable completion, interactive";
  setitup ~complete:true ["-i"];
  present [init; sandbox; complete];
  absent [env_hook];
  step "Enable shell hook, interactive";
  setitup ~hook:true ["-i"];
  present [init; sandbox; complete; env_hook];
  ()

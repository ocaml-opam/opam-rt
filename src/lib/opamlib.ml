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

let default_switch = OpamSwitch.of_string "system"

let repo_opams repo =
  OpamPackage.Map.mapi (fun nv pfx ->
      OpamRepositoryPath.packages repo pfx nv |>
      OpamFileTools.read_opam |>
      function Some x -> x | None -> assert false)
    (OpamRepository.packages_with_prefixes repo)

let exec ?(fake=false) ?(env=[]) opam_root command args =
  OpamConsole.msg "%s\n"
    (OpamConsole.colorise `blue @@ Printf.sprintf ">> %s opam %s %s "
       (String.concat ";" env) command (String.concat " " args));
  let debug = if OpamConsole.debug() then ["--debug"] else [] in
  let args =
    command :: "--yes" :: ["--root"; (OpamFilename.Dir.to_string opam_root)]
    @ debug
    @ (if fake then ["--fake"] else [])
    @ args
  in
  let open OpamProcess.Job.Op in
  OpamProcess.Job.run @@ OpamSystem.make_command
    ~env:(Array.concat [Unix.environment(); Array.of_list env])
    ~verbose:true ~allow_stdin:false
    "opam" args  @@> (fun r ->
      if not (OpamProcess.check_success_and_cleanup r) then
        OpamConsole.msg "Command failed [%d]\n" r.OpamProcess.r_code;
      Done OpamProcess.(r.r_code,r.r_stdout))

let opam ?(fake=false) ?(env=[]) opam_root command args =
  let rcode, _ = exec ~fake ~env opam_root command args in
  if rcode <> 0 then failwith "opam command failed"

let opam_code ?(fake=false) ?(env=[]) opam_root command args =
  fst @@ exec ~fake ~env opam_root command args

let opam_out ?(fake=false) ?(env=[]) opam_root command args =
  snd @@ exec ~fake ~env opam_root command args

let var opam_root var =
  let out = opam_out opam_root "config" ("var" :: [var]) in
  String.concat "\n" out

let init opam_root repo_name repo_url =
  OpamClientConfig.update ();
  opam opam_root "init" [
    OpamRepositoryName.to_string repo_name;
    OpamUrl.to_string repo_url;
    "--no-setup"; "--bare"
  ];
  opam opam_root "switch" ["create";"system";"--empty"];
  opam opam_root "config" ["set";"ocaml-version";"4.02.1"]

let wrap_oargs other_args args =
  match other_args with
  | None -> args
  | Some oargs -> args @ OpamStd.String.split oargs ' '

let install_code opam_root ?version ?oargs name =
  opam_code opam_root "install"
    (wrap_oargs oargs [
        match version with
        | None -> OpamPackage.Name.to_string name
        | Some v -> OpamPackage.to_string (OpamPackage.create name v)
      ])

let install opam_root ?version ?oargs name =
  ignore @@ install_code opam_root ?version ?oargs name

let install_dir opam_root ?(recs=false) ?subpath ?oargs dir =
  opam opam_root "install"
    (wrap_oargs oargs
       ([OpamFilename.Dir.to_string dir]
        @ (if recs then ["--rec"] else [])
        @ (match subpath with | None -> [] | Some s -> ["--subpath";s])))

let reinstall opam_root ?version ?oargs name =
  opam opam_root "reinstall"
    (wrap_oargs oargs [
        match version with
        | None -> OpamPackage.Name.to_string name
        | Some v -> OpamPackage.to_string (OpamPackage.create name v)
      ])

let remove opam_root ?(auto=false) ?oargs name =
  opam opam_root "remove"
    (wrap_oargs oargs
       ((if auto then ["-a"] else [])
        @ [OpamPackage.Name.to_string name]))

let remove_dir opam_root ?(auto=false) ?(recs=false) ?subpath ?oargs dir =
  opam opam_root "remove"
    (wrap_oargs oargs
       ((if auto then ["-a"] else [])
        @ (if recs then ["--rec"] else [])
        @ (match subpath with | None -> [] | Some s -> ["--subpath";s])
        @ [OpamFilename.Dir.to_string dir]))

let update ?oargs opam_root =
  opam opam_root "update" (wrap_oargs oargs [])

let upgrade opam_root ?fake ?oargs packages =
  opam opam_root ?fake "upgrade"
    (wrap_oargs oargs (List.map OpamPackage.Name.to_string packages))

let pin opam_root ?(recs=false) ?subpath ?(action=false) ?oargs name path =
  opam opam_root "pin"
    (wrap_oargs oargs
       (["add";OpamPackage.Name.to_string name;
         "--kind=local"; OpamFilename.Dir.to_string path]
        @ (if recs then ["--recursive"] else [])
        @ (match subpath with | None -> [] | Some s -> ["--subpath";s])
        @ if action then [] else ["-n"]))

let vpin opam_root ?oargs name version =
  opam opam_root "pin"
    (wrap_oargs oargs
       ["add";"-n";
        OpamPackage.Name.to_string name;
        OpamPackage.Version.to_string version])

let pin_kind opam_root ?(action=false) ?kind ?oargs name target =
  opam opam_root "pin"
    (wrap_oargs oargs
       (["add"; OpamPackage.Name.to_string name; target]
        @ (match kind with None -> [] | Some k -> ["--kind";k])
        @ if action then [] else ["-n"]))

let pin_dir opam_root ?(recs=false) ?subpath ?(action=false) ?kind ?oargs dir =
  opam opam_root "pin"
    (wrap_oargs oargs
       ([OpamFilename.Dir.to_string dir]
        @ (match kind with None -> [] | Some k -> ["--kind";k])
        @ (if recs then ["--rec"] else [])
        @ (match subpath with | None -> [] | Some s -> ["--subpath";s])
        @ if action then [] else ["-n"]))

let unpin opam_root ?(action=false) ?oargs name =
  opam opam_root "unpin"
    (wrap_oargs oargs
       ([OpamPackage.Name.to_string name]
        @ if action then [] else ["-n"]))

let unpin_dir opam_root ?(recs=false) ?subpath ?(action=false) ?oargs dir =
  opam opam_root "unpin"
    (wrap_oargs oargs
       ([OpamFilename.Dir.to_string dir]
        @ (if recs then ["--rec"] else [])
        @ (match subpath with | None -> [] | Some s -> ["--subpath";s])
        @ if action then [] else ["-n"]))

let pin_edit opam_root ?(action=false) ?oargs name write_file =
  let f = OpamSystem.temp_file "opamRT" in
  write_file (OpamFilename.of_string f);
  let env = [Printf.sprintf "OPAMEDITOR=cp -f %s" f] in
  opam opam_root ~env "pin"
    (wrap_oargs oargs
       (["edit"; OpamPackage.Name.to_string name]
        @ if action then [] else ["-n"]))

let pinned opam_root =
  opam_out opam_root "pin" ["list"]
  |> List.map (fun s -> OpamStd.String.split s ' ')

let import opam_root ?fake file =
  opam opam_root ?fake "switch" ["import"; OpamFilename.to_string file]

let export opam_root file =
  opam opam_root "switch" ["export"; OpamFilename.to_string file]


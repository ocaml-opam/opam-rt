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

open OpamFilename.Op
open OpamTypes

exception Not_available
exception Allowed_failure

let log fmt =
  OpamConsole.log "RT" fmt

let ok () =
  OpamConsole.msg "%s\n%!" (OpamConsole.colorise `green "[SUCCESS]")

let error e =
  OpamConsole.msg "%s\n%s %s\n%!"
    (Printexc.to_string e)
    (OpamConsole.colorise `red "[ERROR]")
    (String.concat " " (Array.to_list Sys.argv));
  OpamStd.Sys.exit_because `False

let newline () =
  OpamConsole.msg "\n"

let seed_ref = ref 1664
let set_seed seed = seed_ref := seed
let seed () = !seed_ref

let datadir = ref (OpamFilename.Dir.of_string "data")
let set_datadir d = datadir := d
let data s = OpamFilename.create !datadir (OpamFilename.Base.of_string s)

let run f x =
  let seed = seed () in
  Random.init seed;
  try
    OpamConsole.msg "SEED %d\n" seed;
    f x;
    newline ();
    ok ()
  with e ->
    newline ();
    error e

let step () =
  let i = ref 0 in
  fun msg ->
    incr i;
    OpamConsole.msg "%s %s\n"
      (OpamConsole.colorise `yellow @@ Printf.sprintf ">> step %d <<" !i) msg


(** Config *)

type config = {
  repo_name    : OpamRepositoryName.t;
  repo_root    : dirname;
  repo_url     : url;
  opam_root    : dirname;
  contents_root: dirname;
}

let base_repo_name = OpamRepositoryName.of_string "base"

let create_config kind path =
  if OpamFilename.exists_dir path then
    OpamConsole.error_and_exit `Configuration_error
      "%s already exists." (OpamFilename.Dir.to_string path);
  OpamFilename.mkdir path;
  let repo_root = path / "repo" in
  let opam_root = path / "opam" in
  let contents_root = path / "contents" in
  let repo_name = base_repo_name in
  let repo_url =
    match kind with
    | Some `git   ->
      { OpamUrl.backend = `git;
        transport = "file";  path = OpamFilename.Dir.to_string repo_root;
        hash = Some Git.test_tag }
    | Some `http  ->
      { OpamUrl.backend = `http;
        transport = "http";  path = "127.0.0.1:1234";
        hash = None }
    | Some `rsync
    | None        ->
      { OpamUrl.backend = `rsync;
        transport = "file";  path = OpamFilename.Dir.to_string repo_root;
        hash = None }
    | _           -> raise Not_available
  in
  { repo_name; repo_root; repo_url; opam_root; contents_root }

let repos_config_file path =
  OpamFile.make (path // "repo-config")

let read_config path =
  if not (OpamFilename.exists_dir path) then
    OpamConsole.error_and_exit `Configuration_error
      "opam-rt has not been initialized properly";
  let repos = OpamFile.Repos_config.read (repos_config_file path) in
  let repo_name = base_repo_name in
  let opam_root = path / "opam" in
  let repo_root = path / "repo" in
  let repo_url, _repo_trust =
    OpamStd.Option.default (OpamUrl.empty, None)
      (OpamRepositoryName.Map.find repo_name repos)
  in
  let contents_root = path / "contents" in
  { repo_name; repo_root; repo_url; opam_root; contents_root }

let write_repo_config path repo_name repo_url =
  OpamFile.Repos_config.write (repos_config_file path)
    (OpamRepositoryName.Map.singleton repo_name (Some repo_url))


(** opam functions *)

type installed = { installed: package_set; installed_roots: package_set }

let read_installed path =
  let opam_root = path / "opam" in
  let st =
    OpamFile.SwitchSelections.read
      (OpamPath.Switch.selections opam_root Opamlib.default_switch)
  in
  { installed = st.sel_installed;
    installed_roots = st.sel_roots; }

let check_installed path  ?(roots = []) wished_list =
  let wished_installed = OpamPackage.Set.of_list wished_list in
  let wished_roots = OpamPackage.Set.of_list roots in
  let { installed; installed_roots } = read_installed path in
  let pkg_list roots list =
    (String.concat " "
       (List.map (fun nv ->
            if OpamPackage.Set.mem nv roots
            then OpamConsole.colorise `underline (OpamPackage.to_string nv)
            else OpamPackage.to_string nv)
           list))
  in
  if OpamPackage.Set.compare installed wished_installed = 0
  && (roots = [] || OpamPackage.Set.compare wished_roots installed_roots = 0)
  then
    OpamConsole.msg "%s installed: %s\n"
      (OpamConsole.colorise `green "[OK]")
      (pkg_list installed_roots wished_list)
  else
    (OpamConsole.msg "%s installed: %s\n       expecting: %s\n"
       (OpamConsole.colorise `red "[FAIL]")
       (pkg_list installed_roots (OpamPackage.Set.elements installed))
       (pkg_list wished_roots wished_list);
     failwith "Installed packages don't match expectations")

let check_pinned path ?kind wished =
  let packages = Opamlib.pinned path in
  let packages =
    match kind with
    | None -> List.map List.hd packages
    | Some k ->
      OpamStd.List.filter_map (fun l ->
          match l with
          | (nv::_::kind::_url::[] | nv::kind::_url::[]) when kind = k ->
            Some nv
          | _ -> None) packages
  in
  let packages =
    List.map OpamPackage.of_string packages
    |> OpamPackage.Set.of_list
  in
  let wished = OpamPackage.Set.of_list wished in
  if OpamPackage.Set.compare wished packages = 0 then
    OpamConsole.msg "%s %spinned: %s\n"
      (OpamConsole.colorise `green "[OK]")
      (match kind with | Some k -> k^"-" | None -> "")
      (OpamPackage.Set.to_string wished)
  else
    (OpamConsole.msg "%s %spinned: %s\n       expecting: %s\n"
       (OpamConsole.colorise `red "[FAIL]")
       (match kind with | Some k -> k^"-" | None -> "")
       (OpamPackage.Set.to_string packages)
       (OpamPackage.Set.to_string wished);
     failwith "Pinned packages don't match expectations")


(** Server *)
let update_server_index repo_root repo_url =
  match repo_url.OpamUrl.backend with
  | `http ->
    OpamFilename.exec repo_root [["opam"; "admin"; "index"]]
  | _ -> ()

let start_file_server repo_root repo_url =
  match repo_url.OpamUrl.backend with
  | `http ->
    let cmd = Filename.concat (Sys.getcwd ()) "opam-rt-server" in
    OpamFilename.mkdir repo_root;
    update_server_index repo_root repo_url;
    let dir = OpamFilename.Dir.to_string repo_root in
    let p =
      OpamProcess.run_background
        (OpamProcess.command ~dir cmd [OpamFilename.Dir.to_string repo_root])
    in
    let stop () =
      try
        Unix.kill p.OpamProcess.p_pid Sys.sigterm;
        ignore (OpamProcess.wait p)
      with Unix.Unix_error _ -> ()
    in
    at_exit stop;
    Unix.sleep 2;
    stop
  | _ -> fun () -> ()

module type TEST = sig
  val name: string
  val init: OpamUrl.backend option -> OpamFilename.Dir.t -> unit
  val run: OpamUrl.backend option -> OpamFilename.Dir.t -> unit
end

(* INIT *)

let init_repo_update_u kind path =
  log "init-repo-update %s\n" (OpamFilename.Dir.to_string path);
  let { repo_name; repo_root; repo_url; opam_root; contents_root } =
    create_config kind path
  in
  OpamConsole.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo_root);
  Init.create_repo_with_history repo_root contents_root;
  write_repo_config path repo_name (repo_url, None);
  let stop_server = start_file_server repo_root repo_url in
  try
    OpamConsole.msg
      "Initializing an opam instance in %s/ ...\n"
      (OpamFilename.Dir.to_string opam_root);
    Opamlib.init opam_root repo_name repo_url;
    Opamlib.install opam_root
      ~version:(OpamPackage.Version.of_string "1")
      (OpamPackage.Name.of_string "a");
    stop_server ()
  with e -> stop_server (); raise e

let init_dev_update_u contents_kind path =
  log "init-dev-update %s" (OpamFilename.Dir.to_string path);
  let { repo_name; repo_root; repo_url ; opam_root; contents_root } =
    create_config (Some `rsync)  path
  in
  OpamConsole.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo_root);
  Init.create_simple_repo repo_root contents_root contents_kind;
  write_repo_config path repo_name (repo_url, None);
  Opamlib.init opam_root repo_name repo_url;
  Opamlib.update opam_root

let init_pin_update_u contents_kind path =
  log "init-pin-update";
  init_dev_update_u contents_kind path;
  let config = read_config path in
  let pindir = config.contents_root / "a.0" in
  OpamFilename.move_dir ~src:(config.contents_root / "a.1") ~dst:pindir;
  OpamConsole.msg "Pinning a ...\n";
  Opamlib.pin config.opam_root (OpamPackage.Name.of_string "a") pindir

let init_pin_install_u contents_kind path =
  log "init-pin-install";
  let { repo_name; repo_root; repo_url; opam_root; contents_root } =
    create_config (Some `rsync)  path
  in
  OpamConsole.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo_root);
  Init.create_simple_repo repo_root contents_root contents_kind;
  let packages =
    let a1 = Init.package "a" 1 (Some `rsync) contents_root 442 in
    let a2 = Init.package "a" 2 (Some `rsync) contents_root 443 in
    let b1 = Init.package "b" 1 (Some `rsync) contents_root 444 in
    let b2 = Init.package "b" 2 (Some `rsync) contents_root 445 in
    let b1 = Packages.add_depend b1 "a" ~formula:(Atom (`Eq, FString "1")) in
    let b2 = Packages.add_depend b2 "a" ~formula:(Atom (`Eq, FString "2")) in
    [ a1; a2; b1; b2 ]
  in
  List.iter (Packages.add repo_root contents_root) packages;
  write_repo_config path repo_name (repo_url, None);
  Opamlib.init opam_root repo_name repo_url;
  Opamlib.update opam_root;
  let config = read_config path in
  let pindir = config.contents_root / "a.pinned" in
  OpamFilename.copy_dir ~src:(config.contents_root / "a.1") ~dst:pindir;
  OpamConsole.msg "Pinning a ...\n";
  Opamlib.pin config.opam_root (OpamPackage.Name.of_string "a") pindir

let init_reinstall_u contents_kind path =
  log "init-pin-install";
  let { repo_name; repo_root; repo_url; opam_root; contents_root } =
    create_config (Some `rsync)  path
  in
  OpamConsole.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo_root);
  Init.create_simple_repo repo_root contents_root contents_kind;
  let packages =
    let a = Init.package "a" 1 (Some `rsync) contents_root 450 in
    let b =
      Packages.add_depend_with_runtime_checks opam_root
        (Init.package "b" 1 (Some `rsync) contents_root 451)
        "a"
    in
    let c =
      Packages.add_depend_with_runtime_checks opam_root
        (Init.package "c" 1 (Some `rsync) contents_root 452)
        "b"
    in
    let d =
      Packages.add_depend_with_runtime_checks opam_root
        (Init.package "d" 1 (Some `rsync) contents_root 453)
        "c"
    in
    [ a; b; c; d ]
  in
  List.iter (Packages.add repo_root contents_root) packages;
  write_repo_config path repo_name (repo_url, None);
  Opamlib.init opam_root repo_name repo_url;
  Opamlib.update opam_root

(* TEST RUNS *)

(* Basic repository update test: we verify that the global contents is
   the same as the repository contents after each new commit in the
   repository + upgrade. *)
let test_repo_update_u path =
  log "test-repo-update %s" (OpamFilename.Dir.to_string path);
  let { repo_root; repo_url; opam_root ; _ } = read_config path in
  let commits = List.tl @@ List.rev @@ Git.commits repo_root in
  let stop_server = start_file_server repo_root repo_url in
  (* OpamConsole.msg "Commits:\n  %s\n\n" (String.concat "\n  " commits); *)
  List.iter (fun commit ->
      OpamConsole.msg "%s\n"
        (OpamConsole.colorise `yellow @@ Printf.sprintf "*** %s ***" commit);
      Git.checkout repo_root commit;
      Git.branch repo_root;
      update_server_index repo_root repo_url;
      Opamlib.update opam_root;
      Opamlib.upgrade opam_root [];
      Check.packages repo_root opam_root;
    ) (Init.shuffle commits);
  stop_server ()

(* Basic dev package update test: we install the two packages and
   update their contents *)
let test_dev_update_u path =
  log "test-base-update %s" (OpamFilename.Dir.to_string path);
  let { repo_root; opam_root; _ } =
    read_config path
  in
  let opams = Opamlib.repo_opams repo_root in
  let opams =
    OpamPackage.Map.union (fun _ x -> x) opams @@
    Opamlib.repo_opams
      (OpamPath.Switch.Overlay.dir opam_root Opamlib.default_switch)
  in
  let packages =
    OpamPackage.Map.fold (fun nv opam acc ->
        let url = OpamFile.OPAM.url opam in
        match url with
        | None   -> acc
        | Some u ->
          match OpamUrl.local_dir (OpamFile.URL.url u) with
          | Some dir ->
            (nv, (dir, Init.shuffle (Git.commits dir))) :: acc
          | None ->
            acc
      ) opams []
  in

  (* install the packages *)
  List.iter (fun (nv, _) ->
      OpamConsole.msg "Installing %s.\n" (OpamPackage.to_string nv);
      Opamlib.remove opam_root (OpamPackage.name nv);
      Opamlib.install opam_root (OpamPackage.name nv);
    ) packages;

  (* update and check *)
  List.iter (fun (nv, (dir, commits)) ->
      List.iter (fun commit ->
          OpamConsole.msg "%s\n"
            (OpamConsole.colorise `yellow @@
             Printf.sprintf "%s %s" (OpamPackage.to_string nv) commit);
          Git.checkout dir commit;
          Git.branch dir;
          Opamlib.update opam_root;
          Opamlib.upgrade opam_root [];
          Check.contents opam_root nv (OpamPackage.Map.find nv opams);
        ) commits
    ) packages

let should_fail rcode exit_code =
  if rcode = OpamStd.Sys.get_exit_code exit_code then ()
  else failwith "should fail"

let test_pin_install_u path =
  log "test-pin-update %s" (OpamFilename.Dir.to_string path);
  let { repo_root; opam_root; contents_root; _ } =
    read_config path
  in
  let b = OpamPackage.Name.of_string "b" in
  let a = OpamPackage.Name.of_string "a" in
  let v version = OpamPackage.Version.of_string (string_of_int version)
  in
  let (-) = OpamPackage.create in
  let overlay name =
    OpamPath.Switch.Overlay.opam opam_root Opamlib.default_switch name
  in
  let map_overlay f pkg =
    let o = overlay pkg in
    OpamFile.OPAM.write o (f (OpamFile.OPAM.read o))
  in
  let step = step () in
  step "Install b (version 2)";
  Opamlib.install opam_root b;
  check_installed path ~roots:[ b-v 2 ] [ a-v 2; b-v 2 ];
  step "Attempt to install b.1 (should fail because a is pinned to 2)";
  should_fail (Opamlib.install_code opam_root b ~version:(v 1)) `No_solution;
  check_installed path ~roots:[ b-v 2 ] [ a-v 2; b-v 2 ];
  step "Cleanup";
  Opamlib.remove opam_root ~auto:true b;
  check_installed path [];
  step "Change pinned version of a to 1";
  map_overlay (OpamFile.OPAM.with_version (v 1)) a;
  step "Attempt to install b 2";
  should_fail (Opamlib.install_code opam_root b ~version:(v 2)) `No_solution;
  check_installed path [];
  step "Install b, should get version 1";
  Opamlib.install opam_root b;
  check_installed path ~roots:[ b-v 1 ] [ b-v 1; a-v 1 ];
  step "Change pinned version of installed package a back to 2";
  map_overlay (OpamFile.OPAM.with_version (v 2)) a;
  Opamlib.upgrade opam_root [];
  check_installed path ~roots:[ b-v 2 ] [ b-v 2; a-v 2 ];
  (* -- *)
  step "Remove all, unpin a, add a new version of a and update";
  Opamlib.remove opam_root a;
  Opamlib.unpin opam_root a;
  let a3 = Init.package "a" 3 (Some `rsync) contents_root 452 in
  Packages.add repo_root contents_root a3;
  Opamlib.update opam_root;
  check_installed path [];
  step "Pin a to version 2 and install";
  Opamlib.vpin opam_root a (v 2);
  Opamlib.install opam_root a;
  check_installed path ~roots:[a-v 2] [a-v 2];
  step "Unpin a";
  Opamlib.unpin opam_root a;
  check_installed path ~roots:[a-v 2] [a-v 2];
  step "Upgrade";
  Opamlib.upgrade opam_root [];
  check_installed path ~roots:[a-v 3] [a-v 3]

let test_reinstall_u path =
  let { repo_root; opam_root; contents_root; _ } = read_config path in
  let a = OpamPackage.Name.of_string "a" in
  let b = OpamPackage.Name.of_string "b" in
  let c = OpamPackage.Name.of_string "c" in
  let d = OpamPackage.Name.of_string "d" in
  let v version = OpamPackage.Version.of_string (string_of_int version) in
  let (-) = OpamPackage.create in
  let pkg name = name - v 1 in
  let step = step () in
  step "Install d";
  Opamlib.install opam_root d;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Reinstall b";
  Opamlib.reinstall opam_root b;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Install d";
  Opamlib.install opam_root d;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Remove b";
  Opamlib.remove opam_root b;
  check_installed path ~roots:[] (List.map pkg [a]);
  step "Install d";
  Opamlib.install opam_root d;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Remove b from upstream and update";
  OpamFilename.rmdir (path / "repo" / "packages" / "b.1");
  Opamlib.update opam_root;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Upgrade";
  Opamlib.upgrade opam_root [];
  check_installed path ~roots:[] (List.map pkg [a]);
  step "Attempt to reinstall d";
  should_fail (Opamlib.install_code opam_root d) `No_solution;
  step "Revert to the state with all packages installed and b removed upstream";
  let b1 =
    Packages.add_depend_with_runtime_checks opam_root
      (Init.package "b" 1 (Some `rsync) contents_root 451)
      "a"
  in
  Packages.add repo_root contents_root b1;
  Opamlib.update opam_root;
  Opamlib.install opam_root d;
  OpamFilename.rmdir (path / "repo" / "packages" / "b.1");
  Opamlib.update opam_root;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Reinstall c";
  Opamlib.reinstall opam_root c;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Add a new version of c, then upgrade";
  let c2 =
    Packages.add_depend_with_runtime_checks opam_root
      (Init.package "c" 2 (Some `rsync) contents_root 552)
      "b"
  in
  Packages.add repo_root contents_root c2;
  Opamlib.update opam_root;
  Opamlib.upgrade opam_root [c];
  check_installed path ~roots:[pkg d] [a-v 1; b-v 1; c-v 2; d-v 1];
  step "Try to reinstall b (should work using the cache)";
  Opamlib.reinstall opam_root b;
  check_installed path ~roots:[pkg d] [a-v 1; b-v 1; c-v 2; d-v 1];
  step "Try to reinstall a";
  Opamlib.reinstall opam_root a;
  check_installed path ~roots:[pkg d] [a-v 1; b-v 1; c-v 2; d-v 1];
  step "Add a new version of a and upgrade";
  let a2 = Init.package "a" 2 (Some `rsync) contents_root 452 in
  Packages.add repo_root contents_root a2;
  Opamlib.update opam_root;
  Opamlib.upgrade opam_root [];
  check_installed path ~roots:[] [a-v 2];
  step "Remove that new version of a and upgrade";
  OpamFilename.rmdir (path / "repo" / "packages" / "prefix-a" / "a.2");
  Opamlib.update opam_root;
  Opamlib.upgrade opam_root [];
  check_installed path ~roots:[] [a-v 1];
  step "Upgrade again";
  Opamlib.upgrade opam_root [];
  check_installed path ~roots:[] [a-v 1];
  ()

let todo () =
  OpamConsole.msg "%s\n" (OpamConsole.colorise `yellow "[TODO]");
  raise Not_available

let check_and_run kind fn =
  match kind with
  | Some `http -> todo ()
  | _          -> run fn

module Repo_update = struct
  let name = "repo-update"
  let init kind = run (init_repo_update_u kind)
  let run _kind = run test_repo_update_u
end

module Dev_update = struct
  let name = "dev-update"
  let init kind = check_and_run kind (init_dev_update_u kind)
  let run  kind = check_and_run kind test_dev_update_u
end

module Pin_update = struct
  let name = "pin-update"
  let init kind = check_and_run kind (init_pin_update_u kind)
  let run  kind = check_and_run kind test_dev_update_u
end

module Pin_install = struct
  let name = "pin-install"
  let init kind = check_and_run kind (init_pin_install_u kind)
  let run  kind = check_and_run kind test_pin_install_u
end

module Reinstall = struct
  let name = "reinstall"
  let init kind = check_and_run kind (init_reinstall_u kind)
  let run  kind = check_and_run kind test_reinstall_u
end

module Dep_cycle = struct
  let name = "dep-cycle"

  let init_u kind path =
    log "init-dep-cycle";
    let { repo_name; repo_root; repo_url; opam_root; contents_root } =
      create_config (Some `rsync) path
    in
    OpamConsole.msg
      "Creating a new repository in %s/ ...\n"
      (OpamFilename.Dir.to_string repo_root);
    Init.create_simple_repo repo_root contents_root kind;
    let packages =
      let a1 = Init.package "a" 1 (Some `rsync) contents_root 450 in
      let a2 = Init.package "a" 2 (Some `rsync) contents_root 451 in
      let b1 = Init.package "b" 1 (Some `rsync) contents_root 452 in
      let b2 = Init.package "b" 2 (Some `rsync) contents_root 453 in
      let formula =
        Atom (`Eq,
              FIdent (OpamFilter.ident_of_var
                        (OpamVariable.Full.of_string "version")))
      in
      let a1 =
        Packages.add_depend_with_runtime_checks opam_root a1 ~formula "b"
      in
      let b2 =
        Packages.add_depend_with_runtime_checks opam_root b2 ~formula "a"
      in
      [ a1; a2; b1; b2 ]
    in
    List.iter (Packages.add repo_root contents_root) packages;
    write_repo_config path repo_name (repo_url, None);
    Opamlib.init opam_root repo_name repo_url;
    Opamlib.update opam_root

  let init kind = check_and_run kind (init_u kind)

  let run_u _kind path =
    let { opam_root; _ } = read_config path in
    let a = OpamPackage.Name.of_string "a" in
    let b = OpamPackage.Name.of_string "b" in
    let v version = OpamPackage.Version.of_string (string_of_int version) in
    let (-) = OpamPackage.create in
    let step = step () in
    step "Install a1";
    Opamlib.install opam_root a ~version:(v 1);
    check_installed path [a-v 1;b-v 1];
    step "Upgrade";
    Opamlib.upgrade opam_root [];
    check_installed path [a-v 2;b-v 2];
    step "Downgrade a";
    Opamlib.install opam_root a ~version:(v 1);
    check_installed path [a-v 1;b-v 1];
    step "Upgrade";
    Opamlib.upgrade opam_root [];
    check_installed path [a-v 2;b-v 2];
    step "Remove b then downgrade a";
    Opamlib.remove opam_root b;
    Opamlib.install opam_root a ~version:(v 1);
    check_installed path [a-v 1;b-v 1]

  let run kind = check_and_run kind (run_u kind)
end

module Pin_advanced = struct
  let name = "pin-advanced"
  let init kind = check_and_run kind (init_dev_update_u kind)

  let run_u path =
    let { opam_root; contents_root; _ } = read_config path in
    let a = OpamPackage.Name.of_string "a" in
    let z = OpamPackage.Name.of_string "z" in
    let v version = OpamPackage.Version.of_string (string_of_int version) in
    let (-) = OpamPackage.create in
    let step = step () in
    let check_pkg_shares pkg files =
      let files = List.sort compare files in
      let dir =
        OpamFilename.Dir.of_string
          (Opamlib.var opam_root (OpamPackage.Name.to_string pkg ^ ":share"))
      in
      let found_files =
        List.sort compare
          (List.map (OpamFilename.remove_prefix dir)
             (OpamFilename.rec_files dir))
      in
      if files <> found_files then
        (OpamConsole.error
           "Installed files in %s don't match:\n  - found    %s\n  - expected %s"
           (OpamFilename.Dir.to_string dir)
           (String.concat " " found_files) (String.concat ", " files);
         failwith "Bad installed files")
    in
    let write_opam nv ?url touch_files file =
      let this = OpamPackage.Name.to_string (OpamPackage.name nv) in
      OpamFile.OPAM.create nv
      |> OpamFile.OPAM.with_install
        (([CString "mkdir",None;
           CString "-p", None;
           CIdent (this^":share"), None],
          None)::
         List.map (fun f ->
             [CString "touch",None;
              CString ("%{"^this^":share}%/"^f), None],
             None)
           touch_files)
      (* OPAM is not expected to keep track of what to remove after "pin --edit",
         so remove the whole directory. *)
      |> OpamFile.OPAM.with_remove
        [[CString "rm",None; CString "-rf",None;
          CString ("%{"^this^":share}%"), None],None]
      |> OpamFile.OPAM.with_url_opt url
      |> Packages.mandatory_fields "pin"
      |> OpamFile.OPAM.write (OpamFile.make file)
    in
    let tests pin_update pin_target pin_kind pin_version =
      step "Pin (uninstalled) package a";
      Opamlib.pin_kind opam_root ~action:false ~kind:pin_kind a (pin_target a);
      step "Install a";
      Opamlib.install opam_root a;
      check_installed path [a-pin_version];
      check_pkg_shares a ["pinned_5"];
      step "Unpin a";
      Opamlib.unpin opam_root ~action:false a;
      check_installed path [a-pin_version];
      step "Reinstall a (should succeed using the cache)";
      Opamlib.reinstall opam_root a;
      check_installed path [a-pin_version];
      check_pkg_shares a ["pinned_5"];
      step "Upgrade a";
      Opamlib.upgrade opam_root [a];
      check_installed path [a-v 1];
      check_pkg_shares a [];
      step "Pin (installed) package a";
      Opamlib.pin_kind opam_root ~action:true ~kind:pin_kind a (pin_target a);
      check_installed path [a-pin_version];
      check_pkg_shares a ["pinned_5"];
      step "Change in-source opam and update";
      pin_update a (fun () ->
          OpamFilename.remove (OpamFilename.of_string "opam");
          let opdir = OpamFilename.Dir.of_string "opam" in
          OpamFilename.mkdir opdir;
          write_opam (a-v 5) ["repin_5"] (opdir // "opam"));
      Opamlib.upgrade opam_root [a];
      check_installed path [a-pin_version];
      check_pkg_shares a ["repin_5"];
      step "Pin-edit";
      step "Pin-edit AND change in-source opam";
      Opamlib.pin_edit opam_root ~action:false a
        (write_opam
           ~url:(OpamFile.URL.create (OpamUrl.of_string (pin_target a)))
           (a-v 5) ["pin-edit_bis"]);
      pin_update a (fun () ->
          write_opam (a-v 5) ["repin_5bis"]
            (OpamFilename.Dir.of_string "opam" // "opam"));
      (* We are on --yes so the source version should win *)
      Opamlib.upgrade opam_root [a];
      check_installed path [a-pin_version];
      check_pkg_shares a ["repin_5bis"];
      step "Pin-edit with version change";
      Opamlib.pin_edit opam_root ~action:true a
        (write_opam
           ~url:(OpamFile.URL.create (OpamUrl.of_string (pin_target a)))
           (a-v 100) ["pin-edit-v100"]);
      check_installed path [a-v 100];
      check_pkg_shares a ["pin-edit-v100"];
      step "Create new package z by pinning";
      pin_update z (fun () ->
          OpamFilename.write (OpamFilename.of_string "contents") "contents";
          write_opam (z-v 2) ["pkg-b";"no-repo"] (OpamFilename.of_string "opam"));
      Opamlib.pin_kind opam_root ~action:true ~kind:pin_kind z (pin_target z);
      check_installed path [a-v 100; z-v 2];
      check_pkg_shares z ["pkg-b";"no-repo"];
      step "Unpin all";
      Opamlib.unpin opam_root ~action:true a;
      Opamlib.unpin opam_root ~action:true z;
      check_installed path [a-v 1];
      step "Cleanup";
      Opamlib.remove opam_root a
    in

    OpamConsole.header_msg "Local pin";
    let pindir = contents_root / "pins" in
    OpamFilename.mkdir pindir;
    OpamFilename.copy_dir ~src:(contents_root / "a.1") ~dst:(pindir / "a");
    write_opam (a-v 5) ["pinned_5"] (pindir / "a" // "opam");
    let pin_update name f =
      let d = pindir / OpamPackage.Name.to_string name in
      OpamFilename.mkdir d;
      OpamFilename.in_dir d f in
    tests pin_update
      (fun name ->
         "file://" ^
         OpamFilename.Dir.to_string (pindir / OpamPackage.Name.to_string name))
      "path"
      (v 5);

    OpamConsole.header_msg "Git pin";
    let pindir = contents_root / "git-pins" in
    OpamFilename.mkdir pindir;
    let pin_update name f =
      let d = pindir / OpamPackage.Name.to_string name in
      OpamFilename.mkdir d;
      if not (OpamFilename.exists_dir (d/".git")) then
        Git.init d;
      OpamFilename.in_dir d f;
      Git.commit_dir d d "Some commit to %s"
        (OpamPackage.Name.to_string name)
    in
    OpamFilename.copy_dir ~src:(contents_root / "a.1") ~dst:(pindir / "a");
    Git.master (pindir / "a");
    pin_update a (fun () ->
        write_opam (a-v 5) ["pinned_5"] (OpamFilename.of_string "opam"));
    tests pin_update
      (fun name ->
         OpamFilename.Dir.to_string (pindir / OpamPackage.Name.to_string name))
      "git"
      (v 5)

(*  For the moment don't activate it
    OpamConsole.header_msg "Recursive & subpath pinning";
    let a_b = OpamPackage.Name.of_string "a_b" in
    let a_c = OpamPackage.Name.of_string "a_c" in
    let a_d_e = OpamPackage.Name.of_string "a_d_e" in
    let a_f_g_h = OpamPackage.Name.of_string "a_f_g_h" in
    let pkgs_ns = ["a_b"; "a_c"; "a_d_e"  ; "a_f_g_h"] in
    let pkgs_path = ["a_b"; "a_c"; "d/a_d_e"; "f/g/a_f_g_h"] in
    let pkgs_n = a :: (List.map  OpamPackage.Name.of_string pkgs_ns) in
    let pkgs = List.map (fun a -> a-v 4) pkgs_n in
    let top_dir = contents_root / "rec-pins" / "a" in
    OpamFilename.mkdir top_dir;
    Git.init top_dir;
    let orig_pkgs = [
      false, (a-v 4),       (top_dir // "opam");
      true,  (a_c-v 4),     (top_dir /"a_c"//"opam");
      false, (a_b-v 4),     (top_dir / "a_b" // "opam");
      true,  (a_f_g_h-v 4), (top_dir /"f"/"g"/"a_f_g_h"//"opam");
      false, (a_d_e-v 4),   (top_dir / "d" /"a_d_e" // "opam");
    ] in
    List.iter (fun (git,pkg,path) ->
        write_opam pkg ["pinned_4"] path;
        if git then
          Git.commit_file top_dir path
            "add file for %s" (OpamPackage.to_string pkg))
      orig_pkgs;

    let check_install ?(pin=false) tfile pins pkgs =
      if pin then
        List.iter (fun (kind,l) -> check_pinned opam_root ~kind l) pins;
      check_installed path pkgs;
      List.iter (fun a ->  check_pkg_shares a [tfile]) pkgs_n
    in
    let recs = true in
    let pins = [
      "git",   [a_c-v 4; a_f_g_h-v 4];
      "rsync", [a-v 4; a_b-v 4; a_d_e-v 4] ;
    ] in
    let check_install4 ?pin () = check_install ?pin "pinned_4" pins pkgs in
    step "Pin and install recursively";
    Opamlib.pin_dir opam_root ~recs top_dir;
    List.iter (fun a -> Opamlib.install opam_root a) pkgs_n;
    check_install4 ~pin:true ();
    step "Unpin recursively";
    Opamlib.unpin_dir opam_root ~recs top_dir;
    List.iter (fun a -> Opamlib.reinstall opam_root a) pkgs_n;
    check_install4 ();
    step "Remove recursively";
    Opamlib.pin_dir opam_root ~recs top_dir;
    Opamlib.remove_dir ~recs opam_root top_dir;
    check_installed path [];

    step "Pin with subpath";
    Opamlib.install_dir opam_root top_dir;
    List.iter (fun a -> Opamlib.install_dir opam_root ~subpath:a top_dir) pkgs_path;
    check_install4 ~pin:true ();

    step "Update pinned opam";
    let to6 p = (OpamPackage.name p)-v 6 in
    List.iter (fun (git,pkg,path) ->
        write_opam pkg ["pinned_6"] path;
        if git then
          Git.commit_file top_dir path
            "add file for %s" (OpamPackage.to_string pkg))
      (List.map (fun (g,pkg,p) -> g, to6 pkg, p) orig_pkgs);
    let check_install6 ?pin () =
      check_install ?pin "pinned_6"
        (List.map (fun (k,l) -> k, List.map to6 l) pins)
        (List.map to6 pkgs)
    in
    Opamlib.update opam_root;
    Opamlib.upgrade opam_root pkgs_n;
    check_install6 ~pin:true ();

    step "Remove with subpath";
    Opamlib.remove_dir opam_root top_dir;
    List.iter (fun a -> Opamlib.remove_dir opam_root ~subpath:a top_dir) pkgs_path;
    check_installed path []
*)
  let run kind = check_and_run kind run_u
end

module Big_upgrade = struct
  let name = "big-upgrade"

  let init kind path =
    log "init-big-upgrade %s\n" (OpamFilename.Dir.to_string path);
    let { repo_name; repo_root; repo_url; opam_root; _ } =
      create_config kind path
    in
    write_repo_config path repo_name (repo_url, None);
    OpamConsole.msg
      "Creating a new repository in %s/ ...\n"
      (OpamFilename.Dir.to_string repo_root);

    OpamFilename.mkdir repo_root;

    OpamFile.Repo.write
      (OpamFile.make OpamFilename.Op.(repo_root // "repo"))
      (OpamFile.Repo.create ~opam_version:OpamFile.Repo.format_version ());

    OpamFilename.extract_in (data "repo_packages.tar.gz") repo_root;

    let stop_server = start_file_server repo_root repo_url in
    Git.init repo_root;
    let git_dir_re = Re.(compile (str "\\(.*/\\)?\\.git\\(/.*\\)?")) in
    Git.add_list repo_root
      (OpamStd.List.filter_map (fun f ->
           if Re.(all git_dir_re (OpamFilename.to_string f)) <> [] then
             None
           else
             Some f)
          (OpamFilename.rec_files repo_root));
    Git.commit repo_root "Init repo from stored data";
    Git.branch repo_root;
    OpamConsole.msg
      "Initializing an opam instance in %s/ ...\n"
      (OpamFilename.Dir.to_string opam_root);
    Opamlib.init opam_root repo_name repo_url;
    Opamlib.import opam_root ~fake:true (data "init.export");
    stop_server ()

  let check_export opam_root reference =
    let exportfile =
      OpamFilename.of_string (OpamSystem.temp_file "opam-rt-export")
    in
    Opamlib.export opam_root exportfile;
    let ret =
      OpamProcess.run @@
      OpamProcess.command "diff"
        (List.map OpamFilename.to_string [reference; exportfile]) in
    if OpamProcess.check_success_and_cleanup ret then
      (OpamConsole.msg "%s Export files matches reference\n"
         (OpamConsole.colorise `green "[OK]"))
    else
      (OpamConsole.msg "%s Export file differs from %s\n"
         (OpamConsole.colorise `red "[FAIL]")
         (OpamFilename.to_string reference);
       failwith "Installed packages don't match expectations")

  let run _kind path =
    log "test-big-upgrade %s" (OpamFilename.Dir.to_string path);
    let { repo_root; repo_url; opam_root; _ } = read_config path in
    let step = step () in
    let stop_server = start_file_server repo_root repo_url in
    step "update";
    Opamlib.update opam_root;
    check_export opam_root (data "init.export");
    step "upgrade";
    Opamlib.upgrade opam_root ~fake:true [];
    check_export opam_root (data "expected.export");
    stop_server ()
end

let tests =
  List.map (fun m -> let module M = (val m:TEST) in M.name, m) [
    (module Repo_update : TEST);
    (module Dev_update  : TEST);
    (module Pin_update  : TEST);
    (module Pin_install : TEST);
    (module Reinstall   : TEST);
    (module Pin_advanced: TEST);
    (module Dep_cycle   : TEST);
    (module Big_upgrade : TEST);
  ]

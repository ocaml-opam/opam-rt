(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open OpamRTcommon
open OpamTypes
open OpamTypesBase
open OpamStd.Op

exception Not_available
exception Allowed_failure

let exit = OpamStd.Sys.exit

let log fmt =
  OpamConsole.log "RT" fmt

let ok () =
  OpamConsole.msg "%s\n%!" (Color.green "[SUCCESS]")

let error e =
  OpamConsole.msg "%s\n%s %s\n%!"
    (Printexc.to_string e)
    (Color.red "[ERROR]")
    (String.concat " " (Array.to_list Sys.argv));
  exit 1

let newline () =
  OpamConsole.msg "\n"

let run f x =
  let seed = OpamRTcommon.seed () in
  Random.init seed;
  try
    OpamConsole.msg "SEED %d\n" seed;
    f x;
    newline ();
    ok ()
  with e ->
    newline ();
    error e

type config = {
  repo         : repository;
  opam_root    : dirname;
  contents_root: dirname;
}

let create_config kind path =
  if OpamFilename.exists_dir path then
    OpamConsole.error_and_exit "%s already exists." (OpamFilename.Dir.to_string path);
  OpamFilename.mkdir path;
  let repo_name = OpamRepositoryName.of_string "base" in
  let repo_root = path / "repo" in
  let opam_root = path / "opam" in
  let contents_root = path / "contents" in
  let repo_url = match kind with
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
    | _           -> raise Not_available in
  let repo = {
    repo_name;
    repo_root;
    repo_priority = 0;
    repo_url;
  } in
  { repo; opam_root; contents_root }

let repo_config_file path name =
  path / "repo" // (OpamRepositoryName.to_string name^".config")

let read_config path =
  if not (OpamFilename.exists_dir path) then
    OpamConsole.error_and_exit "opam-rt has not been initialized properly";
  let repo =
    OpamFile.Repo_config.read
      (repo_config_file path (OpamRepositoryName.of_string "base")) in
  let opam_root = path / "opam" in
  let contents_root = path / "contents" in
  { repo; opam_root; contents_root }

type installed = { installed: package_set; installed_roots: package_set }
let read_installed path =
  let opam_root = path / "opam" in
  {
    installed = OpamFile.Installed.read
        (OpamPath.Switch.installed opam_root OpamSwitch.system);
    installed_roots = OpamFile.Installed_roots.read
        (OpamPath.Switch.installed_roots opam_root OpamSwitch.system);
  }

let check_installed path  ?(roots = []) wished_list =
  let wished_installed = OpamPackage.Set.of_list wished_list in
  let wished_roots = OpamPackage.Set.of_list roots in
  let { installed; installed_roots } = read_installed path in
  let pkg_list roots list =
    (String.concat " "
       (List.map (fun nv ->
            if OpamPackage.Set.mem nv roots
            then "\027[4m" ^ OpamPackage.to_string nv ^ "\027[m"
            else OpamPackage.to_string nv)
           list))
  in
  if OpamPackage.Set.compare installed wished_installed = 0
  && (roots = [] || OpamPackage.Set.compare wished_roots installed_roots = 0)
  then
    OpamConsole.msg "%s installed: %s\n"
      (Color.green "[OK]")
      (pkg_list installed_roots wished_list)
  else
    (OpamConsole.msg "%s installed: %s\n       expecting: %s\n"
       (Color.red "[FAIL]")
       (pkg_list installed_roots (OpamPackage.Set.elements installed))
       (pkg_list wished_roots wished_list);
     failwith "Installed packages don't match expectations")

let update_server_index repo =
  match repo.repo_url.OpamUrl.backend with
  | `http ->
      OpamFilename.exec repo.repo_root [["opam-admin"; "make"; "--index"]]
  | _ -> ()

let start_file_server repo =
  match repo.repo_url.OpamUrl.backend with
  | `http ->
      let cmd = Filename.concat (Sys.getcwd ()) "file-server" in
      OpamFilename.mkdir repo.repo_root;
      update_server_index repo;
      let dir = OpamFilename.Dir.to_string repo.repo_root in
      let p =
        OpamProcess.run_background
          (OpamProcess.command ~dir cmd [OpamFilename.Dir.to_string repo.repo_root])
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
  let { repo; opam_root; contents_root } = create_config kind path in
  OpamConsole.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo.repo_root);
  OpamRTinit.create_repo_with_history repo contents_root;
  OpamFile.Repo_config.write (repo_config_file path repo.repo_name) repo;
  let stop_server = start_file_server repo in
  OpamConsole.msg
    "Initializing an OPAM instance in %s/ ...\n"
    (OpamFilename.Dir.to_string opam_root);
  OPAM.init opam_root repo;
  OPAM.install opam_root
    ~version:(OpamPackage.Version.of_string "1")
    (OpamPackage.Name.of_string "a");
  stop_server ()

let init_dev_update_u contents_kind path =
  log "init-dev-update %s" (OpamFilename.Dir.to_string path);
  let { repo; opam_root; contents_root } = create_config (Some `rsync)  path in
  OpamConsole.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo.repo_root);
  OpamRTinit.create_simple_repo repo contents_root contents_kind;
  OpamFile.Repo_config.write (repo_config_file path repo.repo_name) repo;
  OPAM.init opam_root repo;
  OPAM.update opam_root

let init_pin_update_u contents_kind path =
  log "init-pin-update";
  init_dev_update_u contents_kind path;
  let config = read_config path in
  let pindir = config.contents_root / "a.0" in
  OpamFilename.move_dir (config.contents_root / "a.1") pindir;
  OpamConsole.msg "Pinning a ...\n";
  OPAM.pin config.opam_root (OpamPackage.Name.of_string "a") pindir

let init_pin_install_u contents_kind path =
  log "init-pin-install";
  let { repo; opam_root; contents_root } = create_config (Some `rsync)  path in
  OpamConsole.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo.repo_root);
  OpamRTinit.create_simple_repo repo contents_root contents_kind;
  let packages =
    let a1 = OpamRTinit.package "a" 1 (Some `rsync) contents_root 442 in
    let a2 = OpamRTinit.package "a" 2 (Some `rsync) contents_root 443 in
    let b1 = OpamRTinit.package "b" 1 (Some `rsync) contents_root 444 in
    let b2 = OpamRTinit.package "b" 2 (Some `rsync) contents_root 445 in
    let b1 = Packages.add_depend b1 "a" ~formula:(Atom (`Eq, OpamPackage.Version.of_string "1")) in
    let b2 = Packages.add_depend b2 "a" ~formula:(Atom (`Eq, OpamPackage.Version.of_string "2")) in
    [ a1; a2; b1; b2 ]
  in
  List.iter (Packages.add repo contents_root) packages;
  OpamFile.Repo_config.write (repo_config_file path repo.repo_name) repo;
  OPAM.init opam_root repo;
  OPAM.update opam_root;
  let config = read_config path in
  let pindir = config.contents_root / "a.pinned" in
  OpamFilename.copy_dir ~src:(config.contents_root / "a.1") ~dst:pindir;
  OpamConsole.msg "Pinning a ...\n";
  OPAM.pin config.opam_root (OpamPackage.Name.of_string "a") pindir

let init_reinstall_u contents_kind path =
  log "init-pin-install";
  let { repo; opam_root; contents_root } = create_config (Some `rsync)  path in
  OpamConsole.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo.repo_root);
  OpamRTinit.create_simple_repo repo contents_root contents_kind;
  let packages =
    let a = OpamRTinit.package "a" 1 (Some `rsync) contents_root 450 in
    let b = Packages.add_depend_with_runtime_checks opam_root
        (OpamRTinit.package "b" 1 (Some `rsync) contents_root 451)
        "a" in
    let c = Packages.add_depend_with_runtime_checks opam_root
        (OpamRTinit.package "c" 1 (Some `rsync) contents_root 452)
        "b" in
    let d = Packages.add_depend_with_runtime_checks opam_root
        (OpamRTinit.package "d" 1 (Some `rsync) contents_root 453)
        "c" in
    [ a; b; c; d ]
  in
  List.iter (Packages.add repo contents_root) packages;
  OpamFile.Repo_config.write (repo_config_file path repo.repo_name) repo;
  OPAM.init opam_root repo;
  OPAM.update opam_root

(* TEST RUNS *)

(* Basic reposiotry update test: we verify that the global contents is
   the same as the repository contents after each new commit in the
   repository + upgrade. *)
let test_repo_update_u path =
  log "test-repo-update %s" (OpamFilename.Dir.to_string path);
  let { repo; opam_root ; _ } = read_config path in
  let commits = Git.commits repo.repo_root in
  let stop_server = start_file_server repo in

  (* OpamConsole.msg "Commits:\n  %s\n\n" (String.concat "\n  " commits); *)
  List.iter (fun commit ->
      OpamConsole.msg "%s\n" (Color.yellow "*** %s ***" commit);
      Git.checkout repo.repo_root commit;
      Git.branch repo.repo_root;
      update_server_index repo;
      OPAM.update opam_root;
      Check.packages repo opam_root;
    ) (OpamRTinit.shuffle commits);

  stop_server ()

(* Basic dev package update test: we install the two packages and
   update their contents *)
let test_dev_update_u path =
  log "test-base-update %s" (OpamFilename.Dir.to_string path);
  let { repo; opam_root; contents_root } = read_config path in
  let packages = OpamRepository.packages repo in
  let packages = OpamPackage.Set.fold (fun nv acc ->
      let url = read_url opam_root nv in
      match url with
      | None   -> acc
      | Some u ->
        match OpamUrl.local_dir (OpamFile.URL.url u) with
        | Some dir ->
          (nv, (dir, OpamRTinit.shuffle (Git.commits dir))) :: acc
        | None ->
          OpamConsole.error_and_exit "Missing contents folder: %s"
            (OpamUrl.base_url (OpamFile.URL.url u))
    ) packages [] in

  (* install the packages *)
  List.iter (fun (nv, _) ->
      OpamConsole.msg "Installing %s.\n" (OpamPackage.to_string nv);
      OPAM.remove opam_root (OpamPackage.name nv);
      OPAM.install opam_root (OpamPackage.name nv);
    ) packages;

  (* update and check *)
  List.iter (fun (nv, (dir, commits)) ->
      List.iter (fun commit ->
          OpamConsole.msg "%s\n"
            (Color.yellow "%s %s" (OpamPackage.to_string nv) commit);
          Git.checkout dir commit;
          Git.branch dir;
          OPAM.update opam_root;
          OPAM.upgrade opam_root [];
          Check.contents opam_root nv;
        ) commits
    ) packages


let test_pin_install_u path =
  log "test-pin-update %s" (OpamFilename.Dir.to_string path);
  let { repo; opam_root; contents_root } = read_config path in
  let b = OpamPackage.Name.of_string "b" in
  let a = OpamPackage.Name.of_string "a" in
  let v version = OpamPackage.Version.of_string (string_of_int version) in
  let (-) = OpamPackage.create in
  let overlay name =
    OpamPath.Switch.Overlay.opam opam_root OpamSwitch.system name in
  let map_overlay f pkg =
    let o = overlay pkg in
    OpamFile.OPAM.write o (f (OpamFile.OPAM.read o)) in
  let step = let i = ref 0 in
    fun msg -> incr i; OpamConsole.msg "%s %s\n" (Color.yellow ">> step %d <<" !i) msg in
  step "Install b (version 2)";
  OPAM.install opam_root b;
  check_installed path ~roots:[ b-v 2 ] [ a-v 2; b-v 2 ];
  step "Attempt to install b.1 (should fail because a is pinned to 2)";
  (try
    OPAM.install opam_root b ~version:(v 1);
    failwith "should fail"
   with OpamSystem.Process_error {OpamProcess.r_code = 3} -> ());
  check_installed path ~roots:[ b-v 2 ] [ a-v 2; b-v 2 ];
  step "Cleanup";
  OPAM.remove opam_root ~auto:true b;
  check_installed path [];
  step "Change pinned version of a to 1";
  map_overlay (fun o -> OpamFile.OPAM.with_version o (v 1)) a;
  step "Attempt to install b 2";
  (try
    OPAM.install opam_root b ~version:(v 2);
    failwith "should fail"
   with OpamSystem.Process_error {OpamProcess.r_code = 3} -> ());
  check_installed path [];
  step "Install b, should get version 1";
  OPAM.install opam_root b;
  check_installed path ~roots:[ b-v 1 ] [ b-v 1; a-v 1 ];
  step "Change pinned version of installed package a back to 2";
  map_overlay (fun o -> OpamFile.OPAM.with_version o (v 2)) a;
  OPAM.upgrade opam_root [];
  check_installed path ~roots:[ b-v 2 ] [ b-v 2; a-v 2 ];
  (* -- *)
  step "Remove all, unpin a, add a new version of a and update";
  OPAM.remove opam_root a;
  OPAM.unpin opam_root a;
  let a3 = OpamRTinit.package "a" 3 (Some `rsync) contents_root 452 in
  Packages.add repo contents_root a3;
  OPAM.update opam_root;
  check_installed path [];
  step "Pin a to version 2 and install";
  OPAM.vpin opam_root a (v 2);
  OPAM.install opam_root a;
  check_installed path ~roots:[a-v 2] [a-v 2];
  step "Unpin a";
  OPAM.unpin opam_root a;
  check_installed path ~roots:[a-v 2] [a-v 2];
  step "Upgrade";
  OPAM.upgrade opam_root [];
  check_installed path ~roots:[a-v 3] [a-v 3]

let test_reinstall_u path =
  let { repo; opam_root; contents_root } = read_config path in
  let a = OpamPackage.Name.of_string "a" in
  let b = OpamPackage.Name.of_string "b" in
  let c = OpamPackage.Name.of_string "c" in
  let d = OpamPackage.Name.of_string "d" in
  let v version = OpamPackage.Version.of_string (string_of_int version) in
  let (-) = OpamPackage.create in
  let pkg name = name - v 1 in
  let step = let i = ref 0 in
    fun msg -> incr i; OpamConsole.msg "%s %s\n" (Color.yellow ">> step %d <<" !i) msg in
  step "Install d";
  OPAM.install opam_root d;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Reinstall b";
  OPAM.reinstall opam_root b;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Install d";
  OPAM.install opam_root d;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Remove b";
  OPAM.remove opam_root b;
  check_installed path ~roots:[] (List.map pkg [a]);
  step "Install d";
  OPAM.install opam_root d;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Remove b from upstream and update";
  OpamSystem.remove_dir (OpamFilename.Dir.to_string (path / "repo" / "packages" / "b.1"));
  OPAM.update opam_root;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Upgrade";
  OPAM.upgrade opam_root [];
  check_installed path ~roots:[] (List.map pkg [a]);
  step "Attempt to reinstall d";
  (try
    OPAM.install opam_root d;
    failwith "should fail"
   with OpamSystem.Process_error {OpamProcess.r_code = 3} -> ());
  step "Revert to the state with all packages installed and b removed upstream";
  let b1 = Packages.add_depend_with_runtime_checks opam_root
      (OpamRTinit.package "b" 1 (Some `rsync) contents_root 451)
      "a" in
  Packages.add repo contents_root b1;
  OPAM.update opam_root;
  OPAM.install opam_root d;
  OpamSystem.remove_dir (OpamFilename.Dir.to_string (path / "repo" / "packages" / "b.1"));
  OPAM.update opam_root;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Reinstall c";
  OPAM.reinstall opam_root c;
  check_installed path ~roots:[pkg d] (List.map pkg [a;b;c;d]);
  step "Add a new version of c, then upgrade";
  let c2 = Packages.add_depend_with_runtime_checks opam_root
      (OpamRTinit.package "c" 2 (Some `rsync) contents_root 552)
      "b" in
  Packages.add repo contents_root c2;
  OPAM.update opam_root;
  OPAM.upgrade opam_root [c];
  check_installed path ~roots:[pkg d] [a-v 1; b-v 1; c-v 2; d-v 1];
  step "Try to reinstall b (should work using the cache)";
  OPAM.reinstall opam_root b;
  check_installed path ~roots:[pkg d] [a-v 1; b-v 1; c-v 2; d-v 1];
  step "Try to reinstall a";
  OPAM.reinstall opam_root a;
  check_installed path ~roots:[pkg d] [a-v 1; b-v 1; c-v 2; d-v 1];
  step "Add a new version of a and upgrade";
  let a2 = OpamRTinit.package "a" 2 (Some `rsync) contents_root 452 in
  Packages.add repo contents_root a2;
  OPAM.update opam_root;
  OPAM.upgrade opam_root [];
  check_installed path ~roots:[] [a-v 2];
  step "Remove that new version of a and upgrade";
  OpamSystem.remove_dir (OpamFilename.Dir.to_string (path / "repo" / "packages" / "prefix-a" / "a.2" ));
  OPAM.update opam_root;
  OPAM.upgrade opam_root [];
  check_installed path ~roots:[] [a-v 1];
  step "Upgrade again";
  OPAM.upgrade opam_root [];
  check_installed path ~roots:[] [a-v 1];
  ()

let todo () =
  OpamConsole.msg "%s\n" (Color.yellow "[TODO]");
  raise Not_available

let check_and_run kind fn =
  match kind with
  | Some `http -> todo ()
  | _          -> run fn

module Repo_update : TEST = struct
  let name = "repo-update"
  let init kind = run (init_repo_update_u kind)
  let run kind = run test_repo_update_u
end

module Dev_update : TEST = struct
  let name = "dev-update"
  let init kind = check_and_run kind (init_dev_update_u kind)
  let run  kind = check_and_run kind test_dev_update_u
end

module Pin_update : TEST = struct
  let name = "pin-update"
  let init kind = check_and_run kind (init_pin_update_u kind)
  let run  kind = check_and_run kind test_dev_update_u
end

module Pin_install : TEST = struct
  let name = "pin-install"
  let init kind = check_and_run kind (init_pin_install_u kind)
  let run  kind = check_and_run kind test_pin_install_u
end

module Reinstall : TEST = struct
  let name = "reinstall"
  let init kind = check_and_run kind (init_reinstall_u kind)
  let run  kind = check_and_run kind test_reinstall_u
end

module Dep_cycle : TEST = struct
  let name = "dep-cycle"

  let init_u kind path =
    log "init-dep-cycle";
    let { repo; opam_root; contents_root } =
      create_config (Some `rsync)  path in
    OpamConsole.msg
      "Creating a new repository in %s/ ...\n"
      (OpamFilename.Dir.to_string repo.repo_root);
    OpamRTinit.create_simple_repo repo contents_root kind;
    let packages =
      let a1 = OpamRTinit.package "a" 1 (Some `rsync) contents_root 450 in
      let a2 = OpamRTinit.package "a" 2 (Some `rsync) contents_root 451 in
      let b1 = OpamRTinit.package "b" 1 (Some `rsync) contents_root 452 in
      let b2 = OpamRTinit.package "b" 2 (Some `rsync) contents_root 453 in
      let a1 = Packages.add_depend_with_runtime_checks opam_root a1
          ~formula:(Atom (`Eq, OpamPackage.Version.of_string "1"))
          "b" in
      let b2 = Packages.add_depend_with_runtime_checks opam_root b2
          ~formula:(Atom (`Eq, OpamPackage.Version.of_string "2"))
          "a" in
      [ a1; a2; b1; b2 ]
    in
    List.iter (Packages.add repo contents_root) packages;
    OpamFile.Repo_config.write (repo_config_file path repo.repo_name) repo;
    OPAM.init opam_root repo;
    OPAM.update opam_root

  let init kind = check_and_run kind (init_u kind)

  let run_u kind path =
    let { repo; opam_root; contents_root } = read_config path in
    let a = OpamPackage.Name.of_string "a" in
    let b = OpamPackage.Name.of_string "b" in
    let v version = OpamPackage.Version.of_string (string_of_int version) in
    let (-) = OpamPackage.create in
    let step = let i = ref 0 in
      fun msg -> incr i; OpamConsole.msg "%s %s\n" (Color.yellow ">> step %d <<" !i) msg in
    step "Install a1";
    OPAM.install opam_root a ~version:(v 1);
    check_installed path [a-v 1;b-v 1];
    step "Upgrade";
    OPAM.upgrade opam_root [];
    check_installed path [a-v 2;b-v 2];
    step "Downgrade a";
    OPAM.install opam_root a ~version:(v 1);
    check_installed path [a-v 1;b-v 1];
    step "Upgrade";
    OPAM.upgrade opam_root [];
    check_installed path [a-v 2;b-v 2];
    step "Remove b then downgrade a";
    OPAM.remove opam_root b;
    OPAM.install opam_root a ~version:(v 1);
    check_installed path [a-v 1;b-v 1]

  let run kind = check_and_run kind (run_u kind)
end

module Pin_advanced : TEST = struct
  let name = "pin-advanced"
  let init kind = check_and_run kind (init_dev_update_u kind)

  let run_u path =
    let { repo; opam_root; contents_root } = read_config path in
    let a = OpamPackage.Name.of_string "a" in
    let z = OpamPackage.Name.of_string "z" in
    let v version = OpamPackage.Version.of_string (string_of_int version) in
    let (-) = OpamPackage.create in
    let step = let i = ref 0 in
      fun msg -> incr i; OpamConsole.msg "%s %s\n" (Color.yellow ">> step %d <<" !i) msg in
    let check_pkg_shares pkg files =
      let files = List.sort compare files in
      let dir =
        OpamFilename.Dir.of_string
          (OPAM.var opam_root (OpamPackage.Name.to_string pkg ^ ":share")) in
      let found_files =
        List.sort compare (List.map
                             (OpamFilename.remove_prefix dir)
                             (OpamFilename.rec_files dir)) in
      if files <> found_files then
        (OpamConsole.error "Installed files in %s don't match:\n  - found    %s\n  - expected %s"
           (OpamFilename.Dir.to_string dir)
           (String.concat " " found_files) (String.concat ", " files);
         failwith "Bad installed files")
    in
    let write_opam nv touch_files file =
      let opam = OpamFile.OPAM.create nv in
      let this = OpamPackage.Name.to_string (OpamPackage.name nv) in
      let opam = OpamFile.OPAM.with_build opam
          (([CString "mkdir",None; CString "-p", None; CIdent (this^":share"), None],None)::
           List.map (fun f -> [CString "touch",None; CString ("%{"^this^":share}%/"^f), None],None)
             touch_files) in
      (* OPAM is not expected to keep track of what to remove after "pin --edit",
         so remove the whole directory. *)
      let opam = OpamFile.OPAM.with_remove opam
          [[CString "rm",None; CString "-rf",None;
            CString ("%{"^this^":share}%"), None],None] in
      OpamFile.OPAM.write file opam
    in
    let tests pin_update pin_target pin_kind pin_version =
      step "Pin (uninstalled) package a";
      OPAM.pin_kind opam_root ~action:false ~kind:pin_kind a (pin_target a);
      step "Install a";
      OPAM.install opam_root a;
      check_installed path [a-pin_version];
      check_pkg_shares a ["pinned_5"];
      step "Unpin a";
      OPAM.unpin opam_root ~action:false a;
      check_installed path [a-pin_version];
      step "Reinstall a (should fail nicely, same version not available)";
      (try
         OPAM.reinstall opam_root a;
         failwith "should fail"
       with OpamSystem.Process_error {OpamProcess.r_code = 66} -> ());
      check_installed path [a-pin_version];
      check_pkg_shares a ["pinned_5"];
      step "Upgrade a";
      OPAM.upgrade opam_root [a];
      check_installed path [a-v 1];
      check_pkg_shares a [];
      step "Pin (installed) package a";
      OPAM.pin_kind opam_root ~action:true ~kind:pin_kind a (pin_target a);
      check_installed path [a-pin_version];
      check_pkg_shares a ["pinned_5"];
      step "Change in-source opam and update";
      pin_update a (fun () ->
          OpamFilename.remove (OpamFilename.of_string "opam");
          let opdir = OpamFilename.Dir.of_string "opam" in
          OpamFilename.mkdir opdir;
          write_opam (a-v 5) ["repin_5"] (opdir // "opam"));
      OPAM.upgrade opam_root [a];
      check_installed path [a-pin_version];
      check_pkg_shares a ["repin_5"];
      step "Pin-edit";
      step "Pin-edit AND change in-source opam";
      OPAM.pin_edit opam_root ~action:false a
        (write_opam (a-v 5) ["pin-edit_bis"]);
      pin_update a (fun () ->
          write_opam (a-v 5) ["repin_5bis"] (OpamFilename.Dir.of_string "opam" // "opam"));
      (* We are on --yes so the source version should win *)
      OPAM.upgrade opam_root [a];
      check_installed path [a-pin_version];
      check_pkg_shares a ["repin_5bis"];
      step "Pin-edit with version change";
      OPAM.pin_edit opam_root ~action:true a
        (write_opam (a-v 100) ["pin-edit-v100"]);
      check_installed path [a-v 100];
      check_pkg_shares a ["pin-edit-v100"];
      step "Create new package z by pinning";
      pin_update z (fun () ->
          OpamFilename.write (OpamFilename.of_string "contents") "contents";
          write_opam (z-v 2) ["pkg-b";"no-repo"] (OpamFilename.of_string "opam"));
      OPAM.pin_kind opam_root ~action:true ~kind:pin_kind z (pin_target z);
      check_installed path [a-v 100; z-v 2];
      check_pkg_shares z ["pkg-b";"no-repo"];
      step "Unpin all";
      OPAM.unpin opam_root ~action:true a;
      OPAM.unpin opam_root ~action:true z;
      check_installed path [a-v 1];
      step "Cleanup";
      OPAM.remove opam_root a
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
         "file://" ^ OpamFilename.Dir.to_string (pindir / OpamPackage.Name.to_string name))
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
        (OpamPackage.Name.to_string name) in
    OpamFilename.copy_dir ~src:(contents_root / "a.1") ~dst:(pindir / "a");
    Git.master (pindir / "a");
    pin_update a (fun () ->
        write_opam (a-v 5) ["pinned_5"] (OpamFilename.of_string "opam"));
    tests pin_update
      (fun name ->
         OpamFilename.Dir.to_string (pindir / OpamPackage.Name.to_string name))
      "git"
      (v 5)

  let run kind = check_and_run kind run_u
end

module Big_upgrade : TEST = struct
  let name = "big-upgrade"

  let init kind path =
    log "init-big-upgrade %s\n" (OpamFilename.Dir.to_string path);
    let { repo; opam_root; contents_root } = create_config kind path in
    OpamFile.Repo_config.write (repo_config_file path repo.repo_name) repo;
    OpamConsole.msg
      "Creating a new repository in %s/ ...\n"
      (OpamFilename.Dir.to_string repo.repo_root);
    OpamFilename.mkdir repo.repo_root;
    OpamSystem.in_dir (OpamFilename.Dir.to_string repo.repo_root) (fun () ->
        OpamSystem.command
          ["tar"; "xzf"; OpamFilename.to_string (data "repo_packages.tar.gz")]);
    let stop_server = start_file_server repo in
    Git.init repo.repo_root;
    let git_dir_re = Re_str.regexp "\\(.*/\\)?\\.git\\(/.*\\)?" in
    Git.add_list repo.repo_root
      (OpamStd.List.filter_map (fun f ->
           if Re_str.string_match git_dir_re f 0 then None
           else Some (OpamFilename.of_string f))
          (OpamSystem.rec_files
             (OpamFilename.Dir.to_string repo.repo_root)));
    Git.commit repo.repo_root "Init repo from stored data";
    Git.branch repo.repo_root;
    OpamConsole.msg
      "Initializing an OPAM instance in %s/ ...\n"
      (OpamFilename.Dir.to_string opam_root);
    OPAM.init opam_root repo;
    OPAM.import opam_root ~fake:true (data "init.export");
    stop_server ()

  let check_export opam_root reference =
    let exportfile = OpamFilename.of_string (OpamSystem.temp_file "opam-rt-export") in
    OPAM.export opam_root exportfile;
    let ret =
      OpamProcess.run
        (OpamProcess.command "diff"
           (List.map OpamFilename.to_string [reference; exportfile])) in
    if ret.OpamProcess.r_code = 0 then
      (OpamConsole.msg "%s Export files matches reference\n"
         (Color.green "[OK]"))
    else
      (OpamConsole.msg "%s Export file differs from %s\n"
         (Color.red "[FAIL]")
         (OpamFilename.to_string reference);
       failwith "Installed packages don't match expectations")

  let run kind path =
    log "test-big-upgrade %s" (OpamFilename.Dir.to_string path);
    let { repo; opam_root; contents_root } = read_config path in
    let step = let i = ref 0 in
      fun msg -> incr i; OpamConsole.msg "%s %s\n" (Color.yellow ">> step %d <<" !i) msg in
    let stop_server = start_file_server repo in
    step "update";
    OPAM.update opam_root;
    check_export opam_root (data "init.export");
    step "upgrade";
    OPAM.upgrade opam_root ~fake:true [];
    try
      check_export opam_root (data "expected.export");
      stop_server ()
    with Failure _ when not (OpamCudf.external_solver_available ()) ->
      OpamConsole.note "Expected failure since the external solver is disabled";
      stop_server ();
      raise Allowed_failure
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

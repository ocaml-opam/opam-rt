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

open OpamFilename.OP
open OpamRTcommon
open OpamTypes
open OpamTypesBase

let log fmt =
  OpamGlobals.log "RT" fmt

let ok () =
  OpamGlobals.msg "%s\n%!" (Color.green "[SUCCESS]")

let error e =
  OpamGlobals.msg "%s\n%s %s\n%!"
    (Printexc.to_string e)
    (Color.red "[ERROR]")
    (String.concat " " (Array.to_list Sys.argv));
  exit 1

let newline () =
  OpamGlobals.msg "\n"

let run f x =
  let seed = OpamRTcommon.seed () in
  Random.init seed;
  try
    OpamGlobals.msg "SEED %d\n" seed;
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
    OpamGlobals.error_and_exit "%s already exists." (OpamFilename.Dir.to_string path);
  OpamFilename.mkdir path;
  let repo_name = OpamRepositoryName.of_string "base" in
  let repo_root = path / "repo" in
  let opam_root = path / "opam" in
  let contents_root = path / "contents" in
  let repo_address = match kind with
    | Some `git   -> OpamFilename.Dir.to_string repo_root, Some Git.test_tag
    | Some `http  -> "http://127.0.0.1:1234", None
    | Some `local
    | None        -> OpamFilename.Dir.to_string repo_root, None
    | _           -> failwith "TODO" in
  let repo_kind = OpamMisc.Option.default `local kind in
  let repo = {
    repo_name;
    repo_root;
    repo_priority = 0;
    repo_address;
    repo_kind;
  } in
  { repo; opam_root; contents_root }

let read_config path =
  if not (OpamFilename.exists_dir path) then
    OpamGlobals.error_and_exit "opam-rt has not been initialized properly";
  let repo =
    let root = path / "repo" in
    let repo = OpamRepository.local root in
    OpamFile.Repo_config.read (OpamPath.Repository.config repo) in
  let opam_root = path / "opam" in
  let contents_root = path / "contents" in
  { repo; opam_root; contents_root }

type installed = { installed: package_set; installed_roots: package_set }
let read_installed path =
  let opam_root = path / "opam" in
  {
    installed = OpamFile.Installed.read
        (OpamPath.Switch.installed opam_root OpamSwitch.default);
    installed_roots = OpamFile.Installed_roots.read
        (OpamPath.Switch.installed_roots opam_root OpamSwitch.default);
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
    OpamGlobals.msg "%s installed: %s\n"
      (Color.green "[OK]")
      (pkg_list installed_roots wished_list)
  else
    (OpamGlobals.msg "%s installed: %s\n       expecting: %s\n"
       (Color.red "[FAIL]")
       (pkg_list installed_roots (OpamPackage.Set.elements installed))
       (pkg_list wished_roots wished_list);
     failwith "Installed packages don't match expectations")

let update_server_index repo =
  match repo.repo_kind with
  | `http -> OpamFilename.exec repo.repo_root [["opam-admin"; "make"; "--index"]]
  | _ -> ()

let stop_file_server repo =
  match repo.repo_kind with
  | `http -> (try OpamSystem.command ["killall"; "file-server"] with _ -> ())
  | _     -> ()

let start_file_server repo =
  match repo.repo_kind with
  | `http ->
    at_exit (fun () -> stop_file_server repo);
    begin match Unix.fork () with
      | -1  -> OpamGlobals.error_and_exit "Fork error"
      | 0   ->
        let cmd = Filename.concat (Sys.getcwd ()) "file-server" in
        Unix.execvp cmd [|cmd; OpamFilename.Dir.to_string repo.repo_root|]
      | pid ->
        update_server_index repo;
        Unix.sleep 2
    end
  | _ -> ()

module type TEST = sig
  val init: OpamTypes.repository_kind option -> OpamFilename.Dir.t -> unit
  val run: OpamTypes.repository_kind option -> OpamFilename.Dir.t -> unit
end

(* INIT *)

let init_repo_update_u kind path =
  log "init-repo-update %s\n" (OpamFilename.Dir.to_string path);
  let { repo; opam_root; contents_root } = create_config kind path in
  OpamGlobals.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo.repo_root);
  OpamRTinit.create_repo_with_history repo contents_root;
  OpamFile.Repo_config.write (OpamPath.Repository.config repo) repo;
  start_file_server repo;
  OpamGlobals.msg
    "Initializing an OPAM instance in %s/ ...\n"
    (OpamFilename.Dir.to_string opam_root);
  OPAM.init opam_root repo;
  OPAM.install opam_root (OpamPackage.Name.of_string "a.1");
  stop_file_server repo

let init_dev_update_u contents_kind path =
  log "init-dev-update %s" (OpamFilename.Dir.to_string path);
  let { repo; opam_root; contents_root } = create_config (Some `local)  path in
  OpamGlobals.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo.repo_root);
  OpamRTinit.create_simple_repo repo contents_root contents_kind;
  OpamFile.Repo_config.write (OpamPath.Repository.config repo) repo;
  OPAM.init opam_root repo;
  OPAM.update opam_root

let init_pin_update_u contents_kind path =
  log "init-pin-update";
  init_dev_update_u contents_kind path;
  let config = read_config path in
  let pindir = config.contents_root / "a.0" in
  OpamFilename.move_dir (config.contents_root / "a.1") pindir;
  OpamGlobals.msg "Pinning a ...\n";
  OPAM.pin config.opam_root (OpamPackage.Name.of_string "a") pindir

let init_pin_install_u contents_kind path =
  log "init-pin-install";
  let { repo; opam_root; contents_root } = create_config (Some `local)  path in
  OpamGlobals.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo.repo_root);
  OpamRTinit.create_simple_repo repo contents_root contents_kind;
  let packages =
    let a1 = OpamRTinit.package "a" 1 (Some `local) contents_root 442 in
    let a2 = OpamRTinit.package "a" 2 (Some `local) contents_root 443 in
    let b1 = OpamRTinit.package "b" 1 (Some `local) contents_root 444 in
    let b2 = OpamRTinit.package "b" 2 (Some `local) contents_root 445 in
    let b1 = Packages.add_depend b1 "a" ~formula:(Atom (`Eq, OpamPackage.Version.of_string "1")) in
    let b2 = Packages.add_depend b2 "a" ~formula:(Atom (`Eq, OpamPackage.Version.of_string "2")) in
    [ a1; a2; b1; b2 ]
  in
  List.iter (Packages.add repo contents_root) packages;
  OpamFile.Repo_config.write (OpamPath.Repository.config repo) repo;
  OPAM.init opam_root repo;
  OPAM.update opam_root;
  let config = read_config path in
  let pindir = config.contents_root / "a.pinned" in
  OpamFilename.copy_dir ~src:(config.contents_root / "a.1") ~dst:pindir;
  OpamGlobals.msg "Pinning a ...\n";
  OPAM.pin config.opam_root (OpamPackage.Name.of_string "a") pindir

let init_reinstall_u contents_kind path =
  log "init-pin-install";
  let { repo; opam_root; contents_root } = create_config (Some `local)  path in
  OpamGlobals.msg
    "Creating a new repository in %s/ ...\n"
    (OpamFilename.Dir.to_string repo.repo_root);
  OpamRTinit.create_simple_repo repo contents_root contents_kind;
  let packages =
    let a = OpamRTinit.package "a" 1 (Some `local) contents_root 450 in
    let b = Packages.add_depend_with_runtime_checks opam_root
        (OpamRTinit.package "b" 1 (Some `local) contents_root 451)
        "a" in
    let c = Packages.add_depend_with_runtime_checks opam_root
        (OpamRTinit.package "c" 1 (Some `local) contents_root 452)
        "b" in
    let d = Packages.add_depend_with_runtime_checks opam_root
        (OpamRTinit.package "d" 1 (Some `local) contents_root 453)
        "c" in
    [ a; b; c; d ]
  in
  List.iter (Packages.add repo contents_root) packages;
  OpamFile.Repo_config.write (OpamPath.Repository.config repo) repo;
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
  start_file_server repo;

  (* OpamGlobals.msg "Commits:\n  %s\n\n" (String.concat "\n  " commits); *)
  List.iter (fun commit ->
      OpamGlobals.msg "%s\n" (Color.yellow "*** %s ***" commit);
      Git.checkout repo.repo_root commit;
      Git.branch repo.repo_root;
      update_server_index repo;
      OPAM.update opam_root;
      Check.packages repo opam_root;
    ) (OpamRTinit.shuffle commits);

  stop_file_server repo

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
        let dir = OpamFilename.Dir.of_string (fst (OpamFile.URL.url u)) in
        if not (OpamFilename.exists_dir dir) then
          OpamGlobals.error_and_exit "Missing contents folder: %s"
            (OpamFilename.Dir.to_string dir);
        (nv, (dir, OpamRTinit.shuffle (Git.commits dir))) :: acc
    ) packages [] in

  (* install the packages *)
  List.iter (fun (nv, _) ->
      OpamGlobals.msg "Installing %s.\n" (OpamPackage.to_string nv);
      OPAM.remove opam_root (OpamPackage.name nv);
      OPAM.install opam_root (OpamPackage.name nv);
    ) packages;

  (* update and check *)
  List.iter (fun (nv, (dir, commits)) ->
      List.iter (fun commit ->
          OpamGlobals.msg "%s\n"
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
  let pinned = OpamPackage.Version.pinned in
  let v version = OpamPackage.Version.of_string (string_of_int version) in
  let (-) = OpamPackage.create in
  let overlay name =
    OpamPath.Switch.Overlay.opam opam_root OpamSwitch.default (OpamPackage.pinned name) in
  let map_overlay f pkg =
    let o = overlay pkg in
    OpamFile.OPAM.write o (f (OpamFile.OPAM.read o)) in
  let step = let i = ref 0 in
    fun msg -> incr i; OpamGlobals.msg "%s %s\n" (Color.yellow ">> step %d <<" !i) msg in
  step "Install b (version 2)";
  OPAM.install opam_root b;
  check_installed path ~roots:[ b-v 2 ] [ a-pinned; b-v 2 ];
  step "Attempt to install b.1 (should fail because a is pinned to 2)";
  (try
    OPAM.install opam_root b ~version:(v 1);
    failwith "should fail"
   with OpamSystem.Process_error {OpamProcess.r_code = 3} -> ());
  check_installed path ~roots:[ b-v 2 ] [ a-pinned; b-v 2 ];
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
  check_installed path ~roots:[ b-v 1 ] [ b-v 1; a-pinned ];
  step "Change pinned version of installed package a back to 2";
  map_overlay (fun o -> OpamFile.OPAM.with_version o (v 2)) a;
  OPAM.upgrade opam_root [];
  check_installed path ~roots:[ b-v 2 ] [ b-v 2; a-pinned ];
  (* -- *)
  step "Remove all, unpin a, add a new version of a and update";
  OPAM.remove opam_root a;
  OPAM.unpin opam_root a;
  let a3 = OpamRTinit.package "a" 3 (Some `local) contents_root 452 in
  Packages.add repo contents_root a3;
  OPAM.update opam_root;
  check_installed path [];
  step "Pin a to version 2 and install";
  OPAM.vpin opam_root a (v 2);
  OPAM.install opam_root a;
  check_installed path ~roots:[a-pinned] [a-pinned];
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
    fun msg -> incr i; OpamGlobals.msg "%s %s\n" (Color.yellow ">> step %d <<" !i) msg in
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
      (OpamRTinit.package "b" 1 (Some `local) contents_root 451)
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
      (OpamRTinit.package "c" 2 (Some `local) contents_root 552)
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
  let a2 = OpamRTinit.package "a" 2 (Some `local) contents_root 452 in
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
  OpamGlobals.msg "%s\n" (Color.yellow "[TODO]");
  exit 0

let check_and_run kind fn =
  match kind with
  | Some `http -> todo ()
  | _          -> run fn

module Repo_update : TEST = struct
  let init kind = run (init_repo_update_u kind)
  let run kind = run test_repo_update_u
end

module Dev_update : TEST = struct
  let init kind = check_and_run kind (init_dev_update_u kind)
  let run  kind = check_and_run kind test_dev_update_u
end

module Pin_update : TEST = struct
  let init kind = check_and_run kind (init_pin_update_u kind)
  let run  kind = check_and_run kind test_dev_update_u
end

module Pin_install : TEST = struct
  let init kind = check_and_run kind (init_pin_install_u kind)
  let run  kind = check_and_run kind test_pin_install_u
end

module Reinstall : TEST = struct
  let init kind = check_and_run kind (init_reinstall_u kind)
  let run  kind = check_and_run kind test_reinstall_u
end

module Big_upgrade : TEST = struct
  let init kind path =
    log "init-big-upgrade %s\n" (OpamFilename.Dir.to_string path);
    let { repo; opam_root; contents_root } = create_config kind path in
    OpamFile.Repo_config.write (OpamPath.Repository.config repo) repo;
    OpamGlobals.msg
      "Creating a new repository in %s/ ...\n"
      (OpamFilename.Dir.to_string repo.repo_root);
    OpamFilename.mkdir repo.repo_root;
    OpamSystem.in_dir (OpamFilename.Dir.to_string repo.repo_root) (fun () ->
        OpamSystem.command
          ["tar"; "xzf"; OpamFilename.to_string (data "repo_packages.tar.gz")]);
    start_file_server repo;
    Git.init repo.repo_root;
    let git_dir_re = Re_str.regexp "\\(.*/\\)?\\.git\\(/.*\\)?" in
    List.iter (fun f ->
        if not (Re_str.string_match git_dir_re f 0) then
          Git.add repo.repo_root (OpamFilename.of_string f))
      (OpamSystem.rec_files (OpamFilename.Dir.to_string repo.repo_root));
    Git.commit repo.repo_root "Init repo from stored data";
    Git.branch repo.repo_root;
    OpamGlobals.msg
      "Initializing an OPAM instance in %s/ ...\n"
      (OpamFilename.Dir.to_string opam_root);
    OPAM.init opam_root repo;
    OPAM.import opam_root ~fake:true (data "init.export");
    stop_file_server repo

  let check_export opam_root reference =
    let exportfile = OpamFilename.of_string (OpamSystem.temp_file "opam-rt-export") in
    OPAM.export opam_root exportfile;
    let ret =
      OpamProcess.run "diff"
        (List.map OpamFilename.to_string [reference; exportfile]) in
    if ret.OpamProcess.r_code = 0 then
      (OpamGlobals.msg "%s Export files matches reference\n"
         (Color.green "[OK]"))
    else
      (OpamGlobals.msg "%s Export file differs from %s\n"
         (Color.red "[FAIL]")
         (OpamFilename.to_string reference);
       failwith "Installed packages don't match expectations")

  let run kind path =
    log "test-big-upgrade %s" (OpamFilename.Dir.to_string path);
    let { repo; opam_root; contents_root } = read_config path in
    let step = let i = ref 0 in
      fun msg -> incr i; OpamGlobals.msg "%s %s\n" (Color.yellow ">> step %d <<" !i) msg in
    step "update";
    OPAM.update opam_root;
    check_export opam_root (data "init.export");
    step "upgrade";
    OPAM.upgrade opam_root ~fake:true [];
    check_export opam_root (data "expected.export")
end

let tests = [
  "repo-update", (module Repo_update : TEST);
  "dev-update",  (module Dev_update  : TEST);
  "pin-update",  (module Pin_update  : TEST);
  "pin-install", (module Pin_install : TEST);
  "reinstall",   (module Reinstall   : TEST);
  "big-upgrade", (module Big_upgrade : TEST);
]

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
    OpamRepositoryName.Map.find repo_name repos
  in
  let contents_root = path / "contents" in
  { repo_name; repo_root; repo_url; opam_root; contents_root }

let write_repo_config path repo_name repo_url =
  OpamFile.Repos_config.write (repos_config_file path)
    (OpamRepositoryName.Map.singleton repo_name repo_url)


(** Init *)

let shuffle l =
  let a = Array.of_list l in
  let permute i j = let x = a.(i) in a.(i) <- a.(j); a.(j) <- x in
  for i = Array.length a - 1 downto 1 do permute i (Random.int (i+1)) done;
  Array.to_list a

let package name version contents_kind contents_root ?(gener_archive=true) seed =
  let pkg = Printf.sprintf "%s.%d" name version in
  let nv = OpamPackage.of_string pkg in
  let contents = Packages.content_create nv seed in
  let files_ = Packages.files seed in
  Packages.({
      nv;
      prefix = prefix nv;
      opam =
        opam nv contents_kind OpamFilename.Op.(contents_root / pkg) seed;
      files = files_;
      contents;
      archive =
        if gener_archive then archive (files_ @ contents) nv seed else None;
    })

let a1 contents_root =
  package "a" 1 (Some `rsync) contents_root

let a2 contents_root =
  package "a" 2 (Some `git) contents_root

let not_very_random n =
  let i = Random.int n in
  if i > Stdlib.(n / 2) then 0 else i

let ar root _ =
  let seed = not_very_random 10 in
  if Random.int 2 = 0 then
    a1 root seed
  else
    a2 root seed

let random_list n fn =
  Array.to_list (Array.init n fn)

(* Create a repository with 2 packages and a complex history *)
let create_repo_with_history repo contents_root =
  OpamFilename.mkdir repo;
  Git.init repo;
  let repo_file =
    OpamFile.Repo.create ~opam_version:OpamFile.Repo.format_version ()
  in
  let repo_filename = OpamRepositoryPath.repo repo in
  OpamFile.Repo.write repo_filename repo_file;
  Git.commit_file repo (OpamFile.filename repo_filename) "Initialise repo";
  let all = [
    a1 contents_root 0;
    a1 contents_root 1;
    a1 contents_root 2;
    a2 contents_root 2;
    a2 contents_root 1;
    a2 contents_root 0;
  ] @ random_list 5 (ar contents_root) in
  List.iter (Packages.add repo contents_root) all;
  Git.branch repo

(* Create a repository with a single package without archive file and
   no history. *)
let create_simple_repo repo contents_root contents_kind =
  OpamFilename.mkdir repo;
  Git.init repo;
  let repo_file =
    OpamFile.Repo.create ~opam_version:OpamFile.Repo.format_version ()
  in
  let repo_filename = OpamRepositoryPath.repo repo in
  OpamFile.Repo.write repo_filename repo_file;
  Git.commit_file repo (OpamFile.filename repo_filename) "Initialise repo";
  let package0 =
    package "a" 1 contents_kind contents_root ~gener_archive:false 10
  in
  Packages.add repo contents_root package0;
  let all =
    package0
    :: random_list 20 (fun _ ->
        package "a" 1 contents_kind contents_root
          ~gener_archive:false (Random.int 20)
      )
  in
  List.iter (fun package ->
      Packages.write repo contents_root package
    ) all;
  Git.branch OpamFilename.Op.(contents_root / "a.1");

(** opam *)

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
      List.filter_map (fun l ->
          match l with
          | nv::"(uninstalled)"::kind::_url::([_;_]|[])
          | nv::kind::_url::([_;_]|[]) ->
            if kind= k then Some nv else None
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

(** Tests *)

let should_fail rcode exit_code =
  if rcode = OpamStd.Sys.get_exit_code exit_code then ()
  else failwith "should fail"

let todo () =
  OpamConsole.msg "%s\n" (OpamConsole.colorise `yellow "[TODO]");
  raise Not_available

let step () =
  let i = ref 0 in
  fun msg ->
    incr i;
    OpamConsole.msg "%s %s\n"
      (OpamConsole.colorise `yellow @@ Printf.sprintf ">> step %d <<" !i) msg

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

let check_and_run kind fn =
  match kind with
  | Some `http -> todo ()
  | _          -> run fn

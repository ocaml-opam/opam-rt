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
open OpamTypes

let name = "pin-advanced"

let init kind = check_and_run kind (Dev_update.init_u kind)

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
    (v 5);

  OpamConsole.header_msg "Working-dir pin";
  let pindir = contents_root / "wd" in
  OpamFilename.mkdir pindir;
  let n = OpamPackage.Name.of_string "workingdir" in
  let v = OpamPackage.Version.of_string "23" in
  let nv = OpamPackage.create n v in
  let script_name = "wd-script" in
  let script_content = "echo 'FOO'" in
  let script = pindir // script_name in
  let opamfile = pindir // "opam" in
  (* opam file *)
  let write_opam build =
    OpamFile.OPAM.create nv
    |> OpamFile.OPAM.with_build build
    |> Packages.mandatory_fields "wd-pin"
    |> OpamFile.OPAM.write (OpamFile.make opamfile)
  in
  write_opam ([[CString "bash", None; CString script_name, None], None]);
  OpamFilename.write script script_content;
  OpamFilename.chmod script 455;
  (* git stuff *)
  Git.init pindir;
  Git.commit_file pindir opamfile "opam file";
  Git.commit_file pindir script "script file";
  step "Initial pin";
  Opamlib.pin_dir opam_root pindir;
  check_pinned opam_root ~kind:"git" [nv];
  Opamlib.install opam_root n;
  check_installed path [nv];
  step "Update working-dir";
  let script_content = "foo" in
  OpamFilename.write script script_content;
  let untracked_name = "untrack" in
  let untracked_content = "echo 'BAZ'" in
  let untracked = pindir // untracked_name in
  OpamFilename.write untracked untracked_content;
  OpamFilename.chmod untracked 455;
  write_opam ([
      [CString "bash", None; CString untracked_name, None], None
    ]);
  Opamlib.install opam_root ~oargs:"--working-dir" n;
  check_installed path [nv];
  let check_files lst =
    OpamStd.List.filter_map (fun (filename, content) ->
        let file =
          opam_root / "system" / ".opam-switch" / "sources" /
          "workingdir" // filename
        in
        if OpamFilename.exists file then
          let read = OpamFilename.read file in
          if read = content then None
          else
            Some (Printf.sprintf "%s: mismatching content %S, expecting %S"
                    filename read content)
        else Some (Printf.sprintf "%s doesn't exists" filename)) lst
  in
  let unvcs_files =
    check_files [
      script_name, script_content;
      untracked_name, untracked_content;
    ]
  in
  if unvcs_files = [] then
    OpamConsole.msg "%s Untracked files copied\n"
      (OpamConsole.colorise `green "[OK]")
  else
    OpamConsole.msg "%s Error on some untracked files:\n%s"
      (OpamConsole.colorise `red "[FAIL]")
      (OpamStd.Format.itemize (fun x -> x) unvcs_files)

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

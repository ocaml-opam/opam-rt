(*
 * Copyright (c) 2013-2015 OCamlPro
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
open OpamTypesBase
open OpamFilename.Op

let seed_ref =
  ref 1664

let set_seed seed =
  seed_ref := seed

let seed () =
  !seed_ref

let datadir = ref (OpamFilename.Dir.of_string "data")

let data s = OpamFilename.create !datadir (OpamFilename.Base.of_string s)

let system_switch = OpamSwitch.of_string "system"

module Color = struct

  let red fmt =
    Printf.ksprintf (fun s -> Printf.sprintf "\027[31m%s\027[m" s) fmt

  let green fmt =
    Printf.ksprintf (fun s -> Printf.sprintf "\027[32m%s\027[m" s) fmt

  let yellow fmt =
    Printf.ksprintf (fun s -> Printf.sprintf "\027[33m%s\027[m" s) fmt

  let blue fmt =
    Printf.ksprintf (fun s -> Printf.sprintf "\027[34m%s\027[m" s) fmt

end

module Git = struct

  let exec repo command =
    OpamFilename.in_dir repo (fun () ->
        OpamSystem.command command
      )

  let return_one_line repo command =
    OpamFilename.in_dir repo (fun () ->
        List.hd (OpamSystem.read_command_output command)
      )

  let return repo command =
    OpamFilename.in_dir repo (fun () ->
        (OpamSystem.read_command_output command)
      )

  let commit repo fmt =
    Printf.kprintf (fun msg ->
        exec repo [ "git"; "commit"; "-a"; "-m"; msg; "--allow-empty" ]
      ) fmt

  let commit_file repo file fmt =
    Printf.kprintf (fun msg ->
        if OpamFilename.exists file then
          let file = OpamFilename.remove_prefix repo file in
          exec repo [ "git"; "add"; file ];
          exec repo [ "git"; "commit"; "-m"; msg; file; "--allow-empty" ];
        else
          OpamConsole.error_and_exit "Cannot commit %s" (OpamFilename.to_string file);
      ) fmt

  let commit_dir repo dir fmt =
    Printf.kprintf (fun msg ->
        if OpamFilename.exists_dir dir then
          let dir =
            OpamStd.String.remove_prefix
              ~prefix:(OpamFilename.to_string (repo//""))
              (OpamFilename.Dir.to_string dir) in
          exec repo [ "git"; "add"; "--all"; dir ];
          exec repo [ "git"; "commit"; "-m"; msg; "--allow-empty" ];
        else
          OpamConsole.error_and_exit "Cannot commit %s"
            (OpamFilename.Dir.to_string dir);
      ) fmt

  let revision repo =
    return_one_line repo [ "git"; "rev-parse"; "HEAD" ]

  let commits repo =
    return repo ["git"; "log"; "master"; "--pretty=format:%H"]

  let init repo =
    exec repo ["git"; "init"]

  let test_tag = "test"

  let branch repo =
    exec repo ["git"; "checkout"; "-B"; test_tag]

  let master repo =
    exec repo ["git"; "checkout"; "master"]

  let add repo file =
    if OpamFilename.exists file then
      let file = OpamFilename.remove_prefix repo file in
      exec repo ["git"; "add"; file]

  let add_list repo files =
    let files = List.filter OpamFilename.exists files in
    let files = List.map (OpamFilename.remove_prefix repo) files in
    (* Add them 100 at a time *)
    let rec take_n l n =
      if n <= 0 then [], l else
        match l with
        | [] -> [], []
        | x::r -> let a,l = take_n r (n-1) in x::a, l
    in
    let rec loop l = match l with
      | [] -> ()
      | l ->
          let files,r = take_n l 100 in
          exec repo ("git" :: "add" :: files);
          loop r
    in
    loop files

  let checkout repo hash =
    exec repo ["git"; "checkout"; hash];
    exec repo ["git"; "clean"; "-fdx"]

  let msg repo commit package fmt =
    Printf.kprintf (fun str ->
        OpamConsole.msg "%-25s %s     %-10s %-30s\n"
          (OpamFilename.Dir.to_string repo)
          commit
          (OpamPackage.to_string package)
          str
      ) fmt

end

let random_string n =
  let s = OpamCompat.Bytes.create n in
  OpamCompat.Bytes.iteri (fun i _ ->
      let c = int_of_char 'A' + Random.int 58 in
      OpamCompat.Bytes.set s i (char_of_int c)
    ) s;
  s

let base = OpamFilename.Base.of_string

module Contents = struct

  let log = OpamConsole.log "CONTENTS"

  type t = (basename * string) list

  let files seed = [
    base "x/a", random_string (1 + seed * 2), 0o644;
    base "x/b", random_string (1 + seed * 3), 0o644;
    base "c"  , random_string (1 + seed), 0o755;
  ]

  let install name =
    base (OpamPackage.Name.to_string name ^ ".install"),
    Printf.sprintf
      "lib: [ \"x/a\" \"x/b\" \"?1\" \"?k/1\" { \"k/1\" }]\n\
       bin: [ \"c\" ]\n",
    0o644

  let create nv seed =
    List.sort compare (install (OpamPackage.name nv) :: files seed)

  let read contents_root nv =
    log "read %s" (OpamPackage.to_string nv);
    let root = contents_root / OpamPackage.to_string nv in
    let files = OpamFilename.rec_files root in
    let files = List.map (fun file ->
        let base = base (OpamFilename.remove_prefix root file) in
        let content = OpamFilename.read file in
        base, content, (Unix.stat (OpamFilename.to_string file)).Unix.st_perm
      ) files in
    List.sort compare files

  let write contents_root nv t =
    log "write %s" (OpamPackage.to_string nv);
    let root = contents_root / OpamPackage.to_string nv in
    if not (OpamFilename.exists_dir root) then (
      OpamFilename.mkdir root;
      Git.init root;
    );
    List.iter (fun (base, contents, mode) ->
        let file = OpamFilename.create root base in
        OpamFilename.write file contents;
        OpamFilename.chmod file mode;
        Git.add root file;
      ) t;
    Git.commit root "Add new content for package %s" (OpamPackage.to_string nv);
    let commit = Git.revision root in
    Git.msg root commit nv "Adding contents"

end

module Packages = struct

  let log = OpamConsole.log "PACKAGES"

  open OpamFile

  type t = {
    nv      : package;
    prefix  : string option;
    opam    : OPAM.t;
    url     : URL.t option;
    descr   : Descr.t option;
    files   : (basename * string * int) list;
    contents: (basename * string * int) list;
    archive : string option;
  }

  let opam nv seed =
    let opam = OPAM.create nv in
    let maintainer = "test-" ^ string_of_int seed in
    OPAM.with_maintainer [maintainer] opam

  let add_depend t ?(formula=OpamFormula.Empty) name =
    let depends =
      OpamFormula.And
        (OPAM.depends t.opam,
         Atom (OpamPackage.Name.of_string name, ([], formula))) in
    { t with opam = OPAM.with_depends depends t.opam }

  let add_depend_with_runtime_checks opam_root t ?formula name =
    let t = add_depend t ?formula name in
    let (/) = Filename.concat in
    let check_cmd =
      let l = [ "test"; "-d"; (OpamFilename.Dir.to_string opam_root/"system"/"lib"/name) ] in
      List.map (fun s -> CString s, None) l, None
    in
    let opam =
      t.opam |>
      OpamFile.OPAM.with_build (OpamFile.OPAM.build t.opam @ [check_cmd]) |>
      OpamFile.OPAM.with_remove (OpamFile.OPAM.remove t.opam @ [check_cmd])
    in
    { t with opam }

  let url kind path = function
    | 0 -> None
    | i ->
      let url = match kind with
        | Some `git   ->
          let u = OpamUrl.parse ~backend:`git (OpamFilename.Dir.to_string path) in
          (* { u with OpamUrl.hash = Some Git.test_tag } *) u
        | None
        | Some `rsync ->
          OpamUrl.parse ~backend:`rsync (OpamFilename.Dir.to_string path)
        | _           -> failwith "TODO" in
      let url_file = URL.create url in
      (* let checksum = Printf.sprintf "%032d" i in *)
      Some ((* URL.with_checksum checksum *) url_file)

  let descr = function
    | 0 -> None
    | i -> Some (Descr.create (Printf.sprintf "This is a very nice package (%d)!" i))

  let archive contents nv seed =
    match seed with
    | 0
    | 1
    | 3 -> None
    | _ ->
      let tmp_file = Filename.temp_file (OpamPackage.to_string nv) "archive" in
      log "Creating an archive file in %s" tmp_file;
      OpamFilename.with_tmp_dir (fun root ->
          let dir = root / OpamPackage.to_string nv in
          List.iter (fun (base, contents, mode) ->
              let file = OpamFilename.create dir base in
              OpamFilename.write file contents;
              OpamFilename.chmod file mode
            ) contents;
          OpamFilename.exec root [
            ["tar"; "czf"; tmp_file; OpamPackage.to_string nv]
          ];
          let contents = OpamSystem.read tmp_file in
          OpamSystem.remove tmp_file;
          Some contents
        )

  let prefix nv =
    match OpamPackage.Version.to_string (OpamPackage.version nv) with
    | "1" -> None
    | _   ->
      let name = OpamPackage.Name.to_string (OpamPackage.name nv) in
      Some (Printf.sprintf "prefix-%s" name)

  let files = function
    | 0 -> []
    | i -> [ (base "1", random_string i, 0o644);
             (base "k/1", random_string (i*2), 0o644) ]

  let file_list repo prefix nv =
    let opam = OpamRepositoryPath.opam repo prefix nv in
    let descr = OpamRepositoryPath.descr repo prefix nv in
    let url = OpamRepositoryPath.url repo prefix nv in
    let files = OpamRepositoryPath.files repo prefix nv in
    let archive = OpamRepositoryPath.archive repo nv in
    opam, descr, url, files, archive

  let file_list_of_t repo t =
    file_list repo t.prefix t.nv

  let write_o f = function
    | None   -> ()
    | Some x -> f x

  let write repo contents_root t =
    let opam, descr, url, files, archive = file_list_of_t repo t in
    List.iter OpamFilename.remove [
        OpamFile.filename opam;
        OpamFile.filename descr;
        OpamFile.filename url;
        archive
    ];
    OpamFilename.rmdir files;
    OPAM.write opam t.opam;
    write_o (Descr.write descr) t.descr;
    write_o (URL.write url) t.url;
    write_o (OpamFilename.write archive) t.archive;
    Contents.write contents_root t.nv t.contents;
    if t.files <> [] then (
      OpamFilename.mkdir files;
      List.iter (fun (base, str, mode) ->
          let file = OpamFilename.create files base in
          OpamFilename.write file str;
          OpamFilename.chmod file mode
        ) t.files
    )

  let read_o f file =
    if OpamFilename.exists file then Some (f file)
    else None

  let read repo contents_root prefix nv =
    let opam, descr, url, files, archive = file_list repo prefix nv in
    let opam = OPAM.read opam in
    let descr = Descr.read_opt descr in
    let url = URL.read_opt url in
    let files =
      if not (OpamFilename.exists_dir files) then []
      else
        let all = OpamFilename.rec_files files in
        List.map (fun file ->
            OpamFilename.Base.of_string (OpamFilename.remove_prefix files file),
            OpamFilename.read file,
            (Unix.stat (OpamFilename.to_string file)).Unix.st_perm
          ) all in
    let contents = Contents.read contents_root nv in
    let archive = read_o OpamFilename.read archive in
    { nv; prefix; opam; descr; url; files; contents; archive }

  let add repo contents_root t =
    write repo contents_root t;
    let opam, descr, url, files, archive = file_list_of_t repo t in
    let commit file =
      if OpamFilename.exists file then (
        Git.add repo.repo_root file;
        Git.commit_file repo.repo_root file
          "Add package %s (%s)"
          (OpamPackage.to_string t.nv) (OpamFilename.to_string file);
        let commit = Git.revision repo.repo_root in
        Git.msg repo.repo_root commit t.nv "Add %s" (OpamFilename.to_string file);
      ) in
    List.iter commit [
      OpamFile.filename opam;
      OpamFile.filename descr;
      OpamFile.filename url;
      archive;
    ];
    if OpamFilename.exists_dir files then (
      let all = OpamFilename.rec_files files in
      List.iter (Git.add repo.repo_root) all;
      Git.commit_dir repo.repo_root files
        "Adding files/* for package %s" (OpamPackage.to_string t.nv);
      let commit = Git.revision repo.repo_root in
      Git.msg repo.repo_root commit t.nv "Add %s" (OpamFilename.Dir.to_string files)
    )

end

module OPAM = struct

  let opam ?(fake=false) ?(env=[]) opam_root command args =
    OpamConsole.msg "%s\n"
      (Color.blue ">> %s opam %s %s "
         (String.concat ";" env)
         command
         (String.concat " " args));
    let debug = if OpamConsole.debug() then ["--debug"] else [] in
    OpamSystem.command
      ~env:(Array.concat [Unix.environment(); Array.of_list env])
      ~verbose:true
      ("opam" :: command ::
         "--yes" ::
         ["--root"; (OpamFilename.Dir.to_string opam_root)]
         @ debug
         @ (if fake then ["--fake"] else [])
         @ args)

  let var opam_root var =
    String.concat "\n"
      (OpamSystem.read_command_output
         ("opam" :: "config" :: "var" ::
          "--root" :: (OpamFilename.Dir.to_string opam_root)
          :: [var]))

  let init opam_root repo =
    OpamClientConfig.update ();
    opam opam_root "init" [
      OpamRepositoryName.to_string repo.repo_name;
      OpamUrl.to_string repo.repo_url;
      "--no-setup"
    ];
    opam opam_root "switch" ["system";"--empty"];
    opam opam_root "config" ["set";"ocaml-version";"4.02.1"]

  let install opam_root ?version name =
    opam opam_root "install" [
      match version with
      | None -> OpamPackage.Name.to_string name
      | Some v -> OpamPackage.to_string (OpamPackage.create name v)
    ]

  let reinstall opam_root ?version name =
    opam opam_root "reinstall" [
      match version with
      | None -> OpamPackage.Name.to_string name
      | Some v -> OpamPackage.to_string (OpamPackage.create name v)
    ]

  let remove opam_root ?(auto=false) name =
    opam opam_root "remove"
      ((if auto then ["-a"] else [])
       @ [OpamPackage.Name.to_string name])

  let update opam_root =
    opam opam_root "update" []

  let upgrade opam_root ?fake packages =
    opam opam_root ?fake "upgrade"
      (List.map OpamPackage.Name.to_string packages)

  let pin opam_root ?(action=false) name path =
    opam opam_root "pin"
      (["add"; OpamPackage.Name.to_string name; "--kind=local"; OpamFilename.Dir.to_string path]
       @ if action then [] else ["-n"])

  let vpin opam_root name version =
    opam opam_root "pin"
      ["add"; "-n"; OpamPackage.Name.to_string name; OpamPackage.Version.to_string version]

  let pin_kind opam_root ?(action=false) ?kind name target =
    opam opam_root "pin"
      (["add"; OpamPackage.Name.to_string name; target]
       @ (match kind with None -> [] | Some k -> ["--kind";k])
       @ if action then [] else ["-n"])

  let unpin opam_root ?(action=false) name =
    opam opam_root "pin"
      (["remove"; OpamPackage.Name.to_string name]
       @ if action then [] else ["-n"])

  let pin_edit opam_root ?(action=false) name write_file =
    let f = OpamSystem.temp_file "opamRT" in
    write_file (OpamFilename.of_string f);
    let env = [Printf.sprintf "OPAMEDITOR=cp -f %s" f] in
    opam opam_root ~env "pin"
      (["edit"; OpamPackage.Name.to_string name]
       @ if action then [] else ["-n"])

  let import opam_root ?fake file =
    opam opam_root ?fake "switch" ["import"; OpamFilename.to_string file]

  let export opam_root file =
    opam opam_root "switch" ["export"; OpamFilename.to_string file]
end

let repo_opams repo =
  OpamPackage.Map.mapi (fun nv pfx ->
      OpamRepositoryPath.packages repo pfx nv |>
      OpamFileHandling.read_opam |>
      function Some x -> x | None -> assert false)
    (OpamRepository.packages_with_prefixes repo)

module Check = struct

  module A = OpamFilename.Attribute

  type error = {
    source: string;
    attr  : file_attribute;
    file  : filename;
  }

  exception Sync_errors of error list

  let sync_errors errors =
    OpamConsole.header_error "Sync mismatch" "%s"
      (String.concat "\n"
         (List.map (fun { source; attr; file } ->
              Printf.sprintf "%s: %s\n  %s: %S\n"
                source (A.to_string attr)
                (OpamFilename.to_string file) (OpamFilename.read file)
            ) errors));
    raise (Sync_errors errors)

  let set map =
    A.Map.fold (fun a _ set -> A.Set.add a set) map A.Set.empty

  exception Found of file_attribute * filename

  let find_binding fn map =
    try A.Map.iter (fun a f -> if fn a f then raise (Found (a,f))) map; raise Not_found
    with Found (a,f) -> (a,f)

  let attributes ?filter dir =
    let filter = match filter with
      | None   -> fun _ -> Some dir
      | Some f -> f in
    let files = OpamFilename.rec_files dir in
    List.fold_left (fun attrs file ->
        match filter file with
        | None     -> attrs
        | Some dir ->
          let attr = OpamFilename.to_attribute dir file in
          A.Map.add attr file attrs
      ) A.Map.empty files

  let sym_diff (name1, a1) (name2, a2) =
    let s1 = set a1 in
    let s2 = set a2 in
    let diff1 = A.Set.diff s1 s2 in
    let diff2 = A.Set.diff s2 s1 in
    let diff = A.Set.union diff1 diff2 in
    A.Set.fold (fun a errors ->
        let source, attr, file =
          if A.Map.mem a a1 then
            (name1, a, A.Map.find a a1)
          else
            (name2, a, A.Map.find a a2) in
        { source; attr; file } :: errors
      ) diff []

  let check_attributes a1 a2 =
    match sym_diff a1 a2 with
    | [] -> ()
    | l  -> sync_errors l

  let check_dirs ?filter (n1, d1) (n2, d2) =
    let a1 = attributes ?filter d1 in
    let a2 = attributes ?filter d2 in
    check_attributes (n1, a1) (n2, a2)

  let installed root =
    let st =
      OpamFile.SwitchSelections.read
        (OpamPath.Switch.selections root system_switch)
    in
    st.sel_installed

  let package_of_filename file =
    let rec aux dirname basename =
      match OpamPackage.of_string_opt (OpamFilename.Base.to_string basename) with
      | None ->
        let basename = OpamFilename.basename_dir dirname in
        let dirname = OpamFilename.dirname_dir dirname in
        aux dirname basename
      | Some nv -> dirname, nv in
    aux (OpamFilename.dirname file) (OpamFilename.basename file)

  let package_of_archivename file =
    let base = OpamFilename.Base.to_string (OpamFilename.basename file) in
    match OpamStd.String.cut_at base '+' with
    | None        -> assert false
    | Some (nv,_) -> OpamPackage.of_string nv

  let check_invariants root =
    let installed = installed root in
    OpamPackage.Set.iter (fun nv ->
        let file = OpamPath.Switch.installed_opam root system_switch nv in
        if not (OpamFile.exists file) then
          OpamConsole.error_and_exit
            "fatal: %s is missing" (OpamFile.to_string file)
      ) installed

  let packages repo root =
    (* invariants *)
    check_invariants root;
    (* metadata *)
    let installed = installed root in
    if OpamPackage.Set.is_empty installed then
      OpamConsole.error_and_exit
        "No package are installed. Tests are meaningless, stopping.";
    let installed_opams =
      OpamPackage.Set.fold (fun nv acc ->
          OpamPackage.Map.add nv
            (OpamFile.OPAM.read
               (OpamPath.Switch.installed_opam root system_switch nv))
            acc)
        installed OpamPackage.Map.empty in
    let repo_opams =
      OpamPackage.Map.filter (fun nv _ -> OpamPackage.Set.mem nv installed)
        (repo_opams repo) in
    let diff = OpamPackage.Map.merge (fun nv a b -> match a,b with
        | Some o1, Some o2 when OpamFile.OPAM.effectively_equal o1 o2 -> None
        | x -> Some x)
        installed_opams repo_opams
    in
    if not (OpamPackage.Map.is_empty diff) then (
      OpamConsole.error "Opam files from repo and installed differ for %s"
        (OpamPackage.Set.to_string (OpamPackage.keys diff));
      OpamPackage.Map.iter (fun nv (installed,repo) ->
          OpamConsole.errmsg "DIFF on %s:\n=== REPO ===\n%s\n=== OPAM ===\n%s\n"
            (OpamPackage.to_string nv)
            (OpamStd.Option.to_string (OpamFile.OPAM.write_to_string ?filename:None) repo)
            (OpamStd.Option.to_string (OpamFile.OPAM.write_to_string ?filename:None) installed))
        diff;
      failwith "Sync error"
    );
    (* archives *)
    let r = OpamRepositoryPath.archives_dir repo in
    let o = OpamPath.archives_dir root in
    let filter file =
      let nv = package_of_archivename file in
      if OpamPackage.Set.mem nv installed
      && OpamFilename.exists (OpamPath.archive root nv) then
        Some (OpamFilename.dirname file)
      else None in
    check_dirs ~filter ("repo", r) ("opam", o)

  let contents opam_root nv opam_file =

    let opam =
      let name = OpamPackage.name nv in
      let libs =
        OpamPath.Switch.Default.lib opam_root system_switch name in
      let bins =
        OpamPath.Switch.Default.bin opam_root system_switch in
      A.Map.union
        (fun x y -> failwith "union")
        (attributes libs)
        (attributes bins) in

    let contents =
      match OpamFile.OPAM.url opam_file with
      | None   -> A.Map.empty
      | Some u ->
        let base =
          match OpamUrl.local_dir (OpamFile.URL.url u) with
          | Some package_root ->
            let filter file =
              if OpamFilename.starts_with (package_root / ".git") file then None
              else if OpamFilename.ends_with ".install" file then None
              else Some (OpamFilename.dirname file) in
            attributes ~filter package_root
          | None -> A.Map.empty
        in
        let files = match OpamFile.OPAM.metadata_dir opam_file with
          | None   -> A.Map.empty
          | Some d -> attributes d in

        A.Map.union (fun x y -> x) files base in

    check_attributes ("opam", opam) ("contents", contents)

end

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
open OpamTypesBase

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

  let exec repo args =
    let open OpamProcess.Job.Op in
    OpamProcess.Job.run @@
    OpamSystem.make_command ~dir:(OpamFilename.Dir.to_string repo)
      "git" args @@> fun r ->
    if not (OpamProcess.check_success_and_cleanup r) then
      failwith (Printf.sprintf "Command failed [%d]: 'git %s'\n%s\n"
        r.OpamProcess.r_code
        (OpamStd.List.to_string (fun x -> x) args)
        (OpamStd.List.to_string (fun x -> x) r.OpamProcess.r_stderr) );
    Done r.OpamProcess.r_stdout

  let git repo args =
    ignore @@ exec repo args

  let git_out repo args =
    exec repo args

  let commit repo fmt =
    Printf.kprintf (fun msg ->
        git repo [ "commit"; "-a"; "-m"; msg; "--allow-empty" ]
      ) fmt

  let commit_file repo file fmt =
    Printf.kprintf (fun msg ->
        if OpamFilename.exists file then
          let file = OpamFilename.remove_prefix repo file in
          git repo [ "add"; file ];
          git repo [ "commit"; "-m"; msg; file; "--allow-empty" ];
        else
          OpamConsole.error_and_exit `Internal_error
            "Cannot commit %s" (OpamFilename.to_string file);
      ) fmt

  let commit_dir repo dir fmt =
    Printf.kprintf (fun msg ->
        if OpamFilename.exists_dir dir then
          let dir = OpamFilename.remove_prefix_dir repo dir in
          let dir = if dir = "" then (OpamFilename.Dir.to_string repo) else dir in
          git repo [ "add"; "--all"; dir ];
          git repo [ "commit"; "-m"; msg; "--allow-empty" ];
        else
          OpamConsole.error_and_exit `Internal_error "Cannot commit %s"
            (OpamFilename.Dir.to_string dir);
      ) fmt

  let revision repo =
  git repo ["log"] ;
    match git_out repo [ "rev-parse"; "HEAD" ] with
    | s::_ -> s
    | [] -> failwith "No empty revision"

  let commits repo =
    git_out repo [ "log"; "master"; "--pretty=format:%H"]

  let init repo =
    git repo [ "init"]

  let test_tag = "test"

  let branch repo =
    git repo [ "checkout"; "-B"; test_tag]

  let master repo =
    git repo [ "checkout"; "master"]

  let add repo file =
    if OpamFilename.exists file then
      let file = OpamFilename.remove_prefix repo file in
      git repo [ "add"; file ]

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
          git repo ("add" :: files);
          loop r
    in
    loop files

  let checkout repo hash =
    git repo [ "checkout"; hash ];
    git repo [ "reset"; "--hard" ];
    git repo [ "clean"; "-fdx" ]

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
  let s = Bytes.create n in
  Bytes.iteri (fun i _ ->
      let c = int_of_char 'A' + Random.int 58 in
      Bytes.set s i (char_of_int c)
    ) s;
  Bytes.to_string s

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
    let root = OpamFilename.Op.(contents_root / OpamPackage.to_string nv) in
    let files = OpamFilename.rec_files root in
    let files = List.map (fun file ->
        let base = base (OpamFilename.remove_prefix root file) in
        let content = OpamFilename.read file in
        base, content, (Unix.stat (OpamFilename.to_string file)).Unix.st_perm
      ) files in
    List.sort compare files

  let write contents_root nv t =
    log "write %s" (OpamPackage.to_string nv);
    let root = OpamFilename.Op.(contents_root / OpamPackage.to_string nv) in
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

  type t = {
    nv      : package;
    prefix  : string option;
    opam    : OpamFile.OPAM.t;
    files   : (basename * string * int) list;
    contents: (basename * string * int) list;
    archive : string option;
  }

  let t_url kind path = function
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
      let url_file = OpamFile.URL.create url in
      (* let checksum = Printf.sprintf "%032d" i in *)
      Some ((* URL.with_checksum checksum *) url_file)

  let t_descr = function
    | 0 -> None
    | i ->
      Some (OpamFile.Descr.create
              (Printf.sprintf "This is a very nice package (%d)!" i))

  let mandatory_fields case ?seed opam =
    let seed =
      match seed with
      | None -> ""
      | Some s -> "-" ^ string_of_int s
    in
    let test_str s = [Printf.sprintf "%s-%s%s" case s seed] in
    let maintainer = test_str "maintainer" in
    let homepage = test_str "homepage" in
    let bug_report = test_str "bug" in
    let author = test_str "authors" in
    let descr = t_descr 4142 in
    let dev_repo = OpamUrl.of_string ("git://dev-repo" ^ seed) in
    opam
    |> OpamFile.OPAM.with_maintainer maintainer
    |> OpamFile.OPAM.with_homepage homepage
    |> OpamFile.OPAM.with_bug_reports bug_report
    |> OpamFile.OPAM.with_author author
    |> OpamFile.OPAM.with_descr_opt descr
    |> OpamFile.OPAM.with_dev_repo dev_repo


  let opam nv kind path seed =
    let opam = OpamFile.OPAM.create nv in
    let url = t_url kind path seed in
    let descr = t_descr seed in
    mandatory_fields "test" ~seed opam
    |> OpamFile.OPAM.with_url_opt url
    |> OpamFile.OPAM.with_descr_opt descr

  let add_depend t ?(formula=OpamFormula.Empty) name =
    let frm = OpamFormula.map (fun a -> Atom (Constraint a)) formula in
    let depends =
      OpamFormula.And
        (OpamFile.OPAM.depends t.opam,
         Atom (OpamPackage.Name.of_string name, frm)) in
    { t with opam = OpamFile.OPAM.with_depends depends t.opam }

  let add_depend_with_runtime_checks opam_root t ?formula name =
    let t = add_depend t ?formula name in
    let check_cmd =
      let l =
        [ "test"; "-d";
          (OpamFilename.(Dir.to_string Op.(opam_root/"system"/"lib"/name))) ]
      in
      List.map (fun s -> CString s, None) l, None
    in
    let opam =
      t.opam |>
      OpamFile.OPAM.with_build (OpamFile.OPAM.build t.opam @ [check_cmd]) |>
      OpamFile.OPAM.with_remove (OpamFile.OPAM.remove t.opam @ [check_cmd])
    in
    { t with opam }

  let archive contents nv seed =
    match seed with
    | 0
    | 1
    | 3 -> None
    | _ ->
      let tmp_file = Filename.temp_file (OpamPackage.to_string nv) "archive" in
      log "Creating an archive file in %s" tmp_file;
      OpamFilename.with_tmp_dir (fun root ->
          let dir = OpamFilename.Op.(root / OpamPackage.to_string nv) in
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
    let files = OpamRepositoryPath.files repo prefix nv in
    let archive =
      OpamFilename.Op.(
        repo / ".." / "repo_archives" // (OpamPackage.to_string nv ^ ".tar.gz"))
    in
    opam, files, archive

  let file_list_of_t repo t =
    file_list repo t.prefix t.nv

  let write_o f = function
    | None   -> ()
    | Some x -> f x

  let write repo contents_root t =
    let opam, files, archive = file_list_of_t repo t in
    List.iter OpamFilename.remove [
      OpamFile.filename opam;
      archive
    ];
    OpamFilename.rmdir files;
    OpamFile.OPAM.write opam t.opam;
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
    let opam, files, archive = file_list repo prefix nv in
    let opam = OpamFile.OPAM.read opam in
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
    { nv; prefix; opam; files; contents; archive }

  let add repo contents_root t =
    write repo contents_root t;
    let opam, files, archive = file_list_of_t repo t in
    let commit file =
      if OpamFilename.exists file then (
        Git.add repo file;
        Git.commit_file repo file
          "Add package %s (%s)"
          (OpamPackage.to_string t.nv) (OpamFilename.to_string file);
        let commit = Git.revision repo in
        Git.msg repo commit t.nv "Add %s" (OpamFilename.to_string file);
      )
    in
    commit (OpamFile.filename opam);
    if OpamFilename.exists_dir files then (
      let all = OpamFilename.rec_files files in
      List.iter (Git.add repo) all;
      Git.commit_dir repo files
        "Adding files/* for package %s" (OpamPackage.to_string t.nv);
      let commit = Git.revision repo in
      Git.msg repo commit t.nv "Add %s" (OpamFilename.Dir.to_string files)
    )

end

module OPAM = struct

  let exec ?(fake=false) ?(env=[]) opam_root command args =
    OpamConsole.msg "%s\n"
      (Color.blue ">> %s opam %s %s "
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

  let install_code opam_root ?version name =
    opam_code opam_root "install" [
      match version with
      | None -> OpamPackage.Name.to_string name
      | Some v -> OpamPackage.to_string (OpamPackage.create name v)
    ]

  let install opam_root ?version name =
    ignore @@ install_code opam_root ?version name

  let install_dir opam_root ?(recs=false) ?subpath dir =
    opam opam_root "install"
      ([OpamFilename.Dir.to_string dir]
       @ (if recs then ["--rec"] else [])
       @ (match subpath with | None -> [] | Some s -> ["--subpath";s]))

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

  let remove_dir opam_root ?(auto=false) ?(recs=false) ?subpath dir =
    opam opam_root "remove"
      ((if auto then ["-a"] else [])
       @ (if recs then ["--rec"] else [])
       @ (match subpath with | None -> [] | Some s -> ["--subpath";s])
       @ [OpamFilename.Dir.to_string dir])

  let update opam_root =
    opam opam_root "update" []

  let upgrade opam_root ?fake packages =
    opam opam_root ?fake "upgrade"
      (List.map OpamPackage.Name.to_string packages)

  let pin opam_root ?(recs=false) ?subpath ?(action=false) name path =
    opam opam_root "pin"
      (["add";OpamPackage.Name.to_string name;
        "--kind=local"; OpamFilename.Dir.to_string path]
       @ (if recs then ["--recursive"] else [])
       @ (match subpath with | None -> [] | Some s -> ["--subpath";s])
       @ if action then [] else ["-n"])

  let vpin opam_root name version =
    opam opam_root "pin"
      ["add";"-n";
       OpamPackage.Name.to_string name;
       OpamPackage.Version.to_string version]

  let pin_kind opam_root ?(action=false) ?kind
      name target =
    opam opam_root "pin"
      (["add"; OpamPackage.Name.to_string name; target]
       @ (match kind with None -> [] | Some k -> ["--kind";k])
       @ if action then [] else ["-n"])

  let pin_dir opam_root ?(recs=false) ?subpath ?(action=false) ?kind
      dir =
    opam opam_root "pin"
      ([OpamFilename.Dir.to_string dir]
       @ (match kind with None -> [] | Some k -> ["--kind";k])
       @ (if recs then ["--rec"] else [])
       @ (match subpath with | None -> [] | Some s -> ["--subpath";s])
       @ if action then [] else ["-n"])

  let unpin opam_root ?(action=false) name =
    opam opam_root "unpin"
      ([OpamPackage.Name.to_string name]
       @ if action then [] else ["-n"])

  let unpin_dir opam_root ?(recs=false) ?subpath ?(action=false) dir =
    opam opam_root "unpin"
      ([OpamFilename.Dir.to_string dir]
       @ (if recs then ["--rec"] else [])
       @ (match subpath with | None -> [] | Some s -> ["--subpath";s])
       @ if action then [] else ["-n"])

  let pin_edit opam_root ?(action=false) name write_file =
    let f = OpamSystem.temp_file "opamRT" in
    write_file (OpamFilename.of_string f);
    let env = [Printf.sprintf "OPAMEDITOR=cp -f %s" f] in
    opam opam_root ~env "pin"
      (["edit"; OpamPackage.Name.to_string name]
       @ if action then [] else ["-n"])

  let pinned opam_root =
    opam_out opam_root "pin" ["list"]
    |> List.map (fun s -> OpamStd.String.split s ' ')

  let import opam_root ?fake file =
    opam opam_root ?fake "switch" ["import"; OpamFilename.to_string file]

  let export opam_root file =
    opam opam_root "switch" ["export"; OpamFilename.to_string file]
end

let repo_opams repo =
  OpamPackage.Map.mapi (fun nv pfx ->
      OpamRepositoryPath.packages repo pfx nv |>
      OpamFileTools.read_opam |>
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
    try
      A.Map.iter (fun a f -> if fn a f then raise (Found (a,f))) map;
      raise Not_found
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

  let check_invariants root =
    let installed = installed root in
    OpamPackage.Set.iter (fun nv ->
        let file = OpamPath.Switch.installed_opam root system_switch nv in
        if not (OpamFile.exists file) then
          OpamConsole.error_and_exit `False
            "fatal: %s is missing" (OpamFile.to_string file)
      ) installed

  let packages repo root =
    (* invariants *)
    check_invariants root;
    (* metadata *)
    let installed = installed root in
    if OpamPackage.Set.is_empty installed then
      OpamConsole.error_and_exit `Configuration_error
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
    )

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
      let base =
        match OpamFile.OPAM.url opam_file with
        | None   -> A.Map.empty
        | Some u ->
          match OpamUrl.local_dir (OpamFile.URL.url u) with
          | Some package_root ->
            let filter file =
              if OpamFilename.starts_with
                  OpamFilename.Op.(package_root / ".git") file
              then None
              else if OpamFilename.ends_with ".install" file then None
              else Some (OpamFilename.dirname file)
            in
            attributes ~filter package_root
          | None -> A.Map.empty
      in
      let files = match OpamFile.OPAM.metadata_dir opam_file with
        | None   -> A.Map.empty
        | Some (_, d) -> attributes OpamFilename.(Op.(Dir.of_string d  / "files")) in

      A.Map.union (fun x y -> x) files base in

    check_attributes ("opam", opam) ("contents", contents)

end

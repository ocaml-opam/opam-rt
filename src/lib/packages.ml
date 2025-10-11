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

open OpamTypes

let log = OpamConsole.log "PACKAGES"

let random_string n =
  let s = Bytes.create n in
  Bytes.iteri (fun i _ ->
      let c = int_of_char 'A' + Random.int 58 in
      Bytes.set s i (char_of_int c)
    ) s;
  Bytes.to_string s

type t = {
  nv      : package;
  prefix  : string option;
  opam    : OpamFile.OPAM.t;
  files   : (basename * string * int) list;
  contents: (basename * string * int) list;
  archive : string option;
}

let t_url kind path seed =
  if seed = 0 then
    None
  else
    let url =
      match kind with
      | Some `git   ->
        let u = OpamUrl.parse ~backend:`git (OpamFilename.Dir.to_string path) in
        (* { u with OpamUrl.hash = Some Git.test_tag } *) u
      | None
      | Some `rsync ->
        OpamUrl.parse ~backend:`rsync (OpamFilename.Dir.to_string path)
      | _           -> failwith "TODO"
    in
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

let add_xfiles files opam =
  let orig =
    match OpamFile.OPAM.extra_files opam with
    | None -> OpamFilename.Base.Map.empty
    | Some xfiles ->
      OpamFilename.Base.Map.of_list xfiles
  in
  let xfiles =
    List.fold_left (fun map file ->
        let bfile =
          let rec aux = function
            | "files"::tl ->
              OpamFilename.Base.of_string
                (OpamStd.List.concat_map ~left:"" ~right:"" ~nil:""
                   ~last_sep:Filename.dir_sep Filename.dir_sep Fun.id tl)
            | _::tl -> aux tl
            | [] -> failwith (OpamFilename.to_string file)
          in
          aux (OpamStd.String.split (OpamFilename.to_string file)
                 Filename.dir_sep.[0])
        in
        OpamFilename.Base.Map.add bfile
          (OpamHash.compute (OpamFilename.to_string file))
          map)
      orig files
  in
  OpamFile.OPAM.with_extra_files
    (OpamFilename.Base.Map.bindings xfiles)
    opam

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
  | 0 | 1 | 3 -> None
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
        Some contents)

let prefix nv =
  match OpamPackage.Version.to_string (OpamPackage.version nv) with
  | "1" -> None
  | _   ->
    let name = OpamPackage.Name.to_string (OpamPackage.name nv) in
    Some (Printf.sprintf "prefix-%s" name)

let files = function
  | 0 -> []
  | i -> [
      (OpamFilename.Base.of_string "1", random_string i, 0o644);
      (OpamFilename.Base.of_string "k/1", random_string (i*2), 0o644)
    ]

let file_list repo prefix nv =
  let opam = OpamRepositoryPath.opam repo prefix nv in
  let files = OpamRepositoryPath.files repo prefix nv in
  let archive =
    let ( /// ) = OpamRepositoryRoot.Dir.Op.( / ) in
    OpamFilename.Op.(
      repo /// ".." / "repo_archives" // (OpamPackage.to_string nv ^ ".tar.gz"))
  in
  opam, files, archive

let file_list_of_t repo t =
  file_list repo t.prefix t.nv

(** Package contents *)
let clog = OpamConsole.log "CONTENTS"

let content_files seed = [
  OpamFilename.Base.of_string "x/a", random_string (1 + seed * 2), 0o644;
  OpamFilename.Base.of_string "x/b", random_string (1 + seed * 3), 0o644;
  OpamFilename.Base.of_string "c"  , random_string (1 + seed),     0o755;
]

let content_install name =
  let install_f =
    OpamFilename.Base.of_string (OpamPackage.Name.to_string name ^ ".install")
  in
  let install_c =
    Printf.sprintf
      "lib: [ \"x/a\" \"x/b\" \"?1\" \"?k/1\" { \"k/1\" }]\n\
       bin: [ \"c\" ]\n"
  in
  install_f,install_c,0o644

let content_create nv seed =
  List.sort compare
    (content_install (OpamPackage.name nv) :: content_files seed)

let content_read contents_root nv =
  clog "read %s" (OpamPackage.to_string nv);
  let root = OpamFilename.Op.(contents_root / OpamPackage.to_string nv) in
  let files = OpamFilename.rec_files root in
  let files =
    List.map (fun file ->
        let base =
          OpamFilename.Base.of_string (OpamFilename.remove_prefix root file)
        in
        let content = OpamFilename.read file in
        base, content, (Unix.stat (OpamFilename.to_string file)).Unix.st_perm
      ) files
  in
  List.sort compare files

let content_write contents_root nv t =
  log "write %s" (OpamPackage.to_string nv);
  let root = OpamFilename.Op.(contents_root / OpamPackage.to_string nv) in
  if not (OpamFilename.exists_dir root) then
    (OpamFilename.mkdir root;
     Git.init root);
  List.iter (fun (base, contents, mode) ->
      let file = OpamFilename.create root base in
      OpamFilename.write file contents;
      OpamFilename.chmod file mode;
      Git.add root file) t;
  Git.commit root "Add new content for package %s" (OpamPackage.to_string nv);
  let commit = Git.revision root in
  Git.msg root commit nv "Adding contents"


(** I/O *)

let write_opt f = function
  | None   -> ()
  | Some x -> f x

let write repo contents_root t =
  let opam, files, archive = file_list_of_t repo t in
  List.iter OpamFilename.remove [
    OpamFile.filename opam;
    archive
  ];
  OpamFilename.rmdir files;
  let opamfile = OpamFile.OPAM.with_extra_files_opt None t.opam in
  write_opt (OpamFilename.write archive) t.archive;
  content_write contents_root t.nv t.contents;
  let opamfile =
    if t.files <> [] then
      (OpamFilename.mkdir files;
       List.fold_left (fun opam (base, str, mode) ->
           let file = OpamFilename.create files base in
           OpamFilename.write file str;
           OpamFilename.chmod file mode;
           add_xfiles [file] opam)
         opamfile t.files)
    else opamfile
  in
  OpamFile.OPAM.write opam opamfile

let read_opt f file =
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
          (Unix.stat (OpamFilename.to_string file)).Unix.st_perm) all
  in
  let contents = content_read contents_root nv in
  let archive = read_opt OpamFilename.read archive in
  { nv; prefix; opam; files; contents; archive }

let add repo contents_root t =
  write repo contents_root t;
  let opam, files, _ = file_list_of_t repo t in
  let opamf = (OpamFile.filename opam) in
  let opamfile_present = OpamFilename.exists opamf in
  let files_present = OpamFilename.exists_dir files in
  if opamfile_present then
    (let repo = OpamRepositoryRoot.Dir.to_dir repo in
     Git.add repo opamf;
     if files_present then Git.add_dir repo files;
     Git.commit repo "Add package %s (%s%s)"
       (OpamPackage.to_string t.nv)
       (OpamFilename.to_string opamf)
       (if files_present then " and files/*" else "");
     let commit = Git.revision repo in
     Git.msg repo commit t.nv "Add %s%s"
       (OpamFilename.to_string opamf)
       (if files_present then " and files/*" else ""))

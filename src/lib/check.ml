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

module Attr = OpamFilename.Attribute

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
              source (Attr.to_string attr)
              (OpamFilename.to_string file) (OpamFilename.read file)
          ) errors));
  raise (Sync_errors errors)

let set map =
  Attr.Map.fold (fun a _ set -> Attr.Set.add a set) map Attr.Set.empty

exception Found of file_attribute * filename

let _find_binding fn map =
  try
    Attr.Map.iter (fun a f -> if fn a f then raise (Found (a,f))) map;
    raise Not_found
  with Found (a,f) -> (a,f)

let attributes ?filter dir =
  let filter =
    match filter with
    | None   -> fun _ -> Some dir
    | Some f -> f
  in
  let files = OpamFilename.rec_files dir in
  List.fold_left (fun attrs file ->
      match filter file with
      | None     -> attrs
      | Some dir ->
        let attr = OpamFilename.to_attribute dir file in
        Attr.Map.add attr file attrs
    ) Attr.Map.empty files

let sym_diff (name1, a1) (name2, a2) =
  let s1 = set a1 in
  let s2 = set a2 in
  let diff1 = Attr.Set.diff s1 s2 in
  let diff2 = Attr.Set.diff s2 s1 in
  let diff = Attr.Set.union diff1 diff2 in
  Attr.Set.fold (fun a errors ->
      let source, attr, file =
        if Attr.Map.mem a a1 then
          (name1, a, Attr.Map.find a a1)
        else
          (name2, a, Attr.Map.find a a2)
      in
      { source; attr; file } :: errors
    ) diff []

let check_attributes a1 a2 =
  match sym_diff a1 a2 with
  | [] -> ()
  | l  -> sync_errors l

let _check_dirs ?filter (n1, d1) (n2, d2) =
  let a1 = attributes ?filter d1 in
  let a2 = attributes ?filter d2 in
  check_attributes (n1, a1) (n2, a2)

let installed root =
  let st =
    OpamFile.SwitchSelections.read
      (OpamPath.Switch.selections root Opamlib.default_switch)
  in
  st.sel_installed

let _package_of_filename file =
  let rec aux dirname basename =
    match OpamPackage.of_string_opt (OpamFilename.Base.to_string basename) with
    | None ->
      let basename = OpamFilename.basename_dir dirname in
      let dirname = OpamFilename.dirname_dir dirname in
      aux dirname basename
    | Some nv -> dirname, nv
  in
  aux (OpamFilename.dirname file) (OpamFilename.basename file)

let check_invariants root =
  let installed = installed root in
  OpamPackage.Set.iter (fun nv ->
      let file =
        OpamPath.Switch.installed_opam root Opamlib.default_switch nv
      in
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
             (OpamPath.Switch.installed_opam root Opamlib.default_switch nv))
          acc)
      installed OpamPackage.Map.empty
  in
  let repo_opams =
    OpamPackage.Map.filter (fun nv _ -> OpamPackage.Set.mem nv installed)
      (Opamlib.repo_opams repo)
  in
  let diff = OpamPackage.Map.merge (fun _nv a b -> match a,b with
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
          (OpamStd.Option.to_string
             (OpamFile.OPAM.write_to_string ?filename:None) repo)
          (OpamStd.Option.to_string
             (OpamFile.OPAM.write_to_string ?filename:None) installed))
      diff;
    failwith "Sync error"
  )

let contents opam_root nv opam_file =
  let opam =
    let name = OpamPackage.name nv in
    let libs =
      OpamPath.Switch.Default.lib opam_root Opamlib.default_switch name
    in
    let bins =
      OpamPath.Switch.Default.bin opam_root Opamlib.default_switch
    in
    Attr.Map.union (fun _ _ -> failwith "union")
      (attributes libs)
      (attributes bins)
  in
  let contents =
    let base =
      match OpamFile.OPAM.url opam_file with
      | None   -> Attr.Map.empty
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
        | None -> Attr.Map.empty
    in
    let files =
      match OpamFile.OPAM.metadata_dir opam_file with
      | None   -> Attr.Map.empty
      | Some d -> attributes OpamFilename.Op.(d  / "files")
    in
    Attr.Map.union (fun x _ -> x) files base
  in
  check_attributes ("opam", opam) ("contents", contents)

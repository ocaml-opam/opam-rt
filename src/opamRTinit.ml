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

open OpamRTcommon
open OpamTypes
open OpamFilename.OP

let shuffle l =
  let a = Array.init (List.length l) (fun _ -> None) in
  let rec aux n = function
    | []   -> ()
    | h::t ->
      let i = ref (Random.int n) in
      while a.(!i mod Array.length a) <> None do incr i done;
      a.(!i) <- Some h;
      aux (n-1) t in
  aux (List.length l) l;
  Array.fold_left (fun acc -> function
      | None   -> assert false
      | Some i -> i :: acc
    )  [] a

let package name version contents_kind contents_root ?(gener_archive=true) seed =
  let pkg = Printf.sprintf "%s.%d" name version in
  let nv = OpamPackage.of_string pkg in
  let contents = Contents.create nv seed in
  let files_ = Packages.files seed in
  Packages.({
    nv;
    prefix   = prefix nv;
    opam     = opam nv seed;
    url      = url contents_kind (contents_root / pkg) seed;
    descr    = descr seed;
    files    = files_;
    contents;
    archive  = if gener_archive then archive (files_ @ contents) nv seed else None;
  })

let a1 contents_root =
  package "a" 1 (Some `local) contents_root

let a2 contents_root =
  package "a" 2 (Some `git) contents_root

let not_very_random n =
  let i = Random.int n in
  if i > Pervasives.(/) n 2 then 0 else i

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
  OpamFilename.mkdir repo.repo_root;
  Git.init repo.repo_root;
  let all = [
    a1 contents_root 0;
    a1 contents_root 1;
    a1 contents_root 2;
    a2 contents_root 2;
    a2 contents_root 1;
    a2 contents_root 0;
  ] @ random_list 5 (ar repo.repo_root) in
  List.iter (Packages.add repo contents_root) all;
  Git.branch repo.repo_root

(* Create a repository with a single package without archive file and
   no history. *)
let create_simple_repo repo contents_root contents_kind =
  OpamFilename.mkdir repo.repo_root;
  Git.init repo.repo_root;
  let package0 = package "a" 1 contents_kind contents_root ~gener_archive:false 10 in
  Packages.add repo contents_root package0;
  let all =
    package0
    :: random_list 20 (fun _ ->
        package "a" 1 contents_kind contents_root ~gener_archive:false (Random.int 20)
      ) in
  List.iter (fun package ->
      Packages.write repo contents_root package
    ) all;
  Git.branch (contents_root / "a.1");
  Git.commit repo.repo_root "Add package"

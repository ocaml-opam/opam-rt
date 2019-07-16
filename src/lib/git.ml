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

let exec repo args =
  let open OpamProcess.Job.Op in
  OpamProcess.Job.run @@
  OpamSystem.make_command ~dir:(OpamFilename.Dir.to_string repo)
    "git" args @@> fun r ->
  if not (OpamProcess.check_success_and_cleanup r) then
    failwith (Printf.sprintf "Command failed [%d]: 'git %s'\n%s\n"
                r.OpamProcess.r_code
                (OpamStd.List.to_string (fun x -> x) args)
                (OpamStd.List.to_string (fun x -> x) r.OpamProcess.r_stderr));
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
  let rec loop l =
    match l with
    | [] -> ()
    | l ->
      let files,r = take_n l 100
      in
      git repo ("add":: files);
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

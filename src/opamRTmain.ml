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
open Cmdliner

let version = "0.0.1"

(* Help sections common to all commands *)
let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";

  `S "FURTHER DOCUMENTATION";
  `P (Printf.sprintf "See https://opam.ocaml.org.");

  `S "AUTHOR";
  `P "Thomas Gazagnaire   <thomas.gazagnaire@ocamlpro.com>"; `Noblank;
  `P "Louis Gesbert       <louis.gesbert@ocamlpro.com>"; `Noblank;
]

let dirname =
  let parse str = `Ok (OpamFilename.Dir.of_string str) in
  let print ppf dir = Format.pp_print_string ppf (OpamFilename.prettify_dir dir) in
  parse, print

(* HELP *)
let help =
  let doc = "Display help about opam-rt and opam-rt commands." in
  let man = [
    `S "DESCRIPTION";
    `P "Prints help about opam-rt commands.";
    `P "Use `$(mname) help topics' to get the full list of help topics.";
  ] in
  let topic =
    let doc = Arg.info [] ~docv:"TOPIC" ~doc:"The topic to get help on." in
    Arg.(value & pos 0 (some string) None & doc )
  in
  let help man_format cmds topic = match topic with
    | None       -> `Help (`Pager, None)
    | Some topic ->
      let topics = "topics" :: cmds in
      let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
      match conv topic with
      | `Error e -> `Error (false, e)
      | `Ok t when t = "topics" -> List.iter print_endline cmds; `Ok ()
      | `Ok t -> `Help (man_format, Some t) in

  Term.(ret (pure help $Term.man_format $Term.choice_names $topic)),
  Term.info "help" ~doc ~man

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~doc ~man title

let test_case =
  let doc = Arg.info ~docv:"TEST" ~doc:"Name of the test to run." [] in
  let case = Arg.enum OpamRT.tests in
  Arg.(required & pos 1 (some case) None & doc)

let seed_flag =
  let doc = Arg.info ~docv:"SEED" ~doc:"Value of the random seed." ["S";"seed"] in
  Arg.(value & opt int 1664 & doc)

let set_seed seed =
  OpamRTcommon.set_seed seed

let data_dir =
  let doc = "Set the directory where the data for the some tests can be found" in
  Arg.(value & opt dir "data" & info ~doc ["--data"])

let apply_data_dir data_dir =
  OpamRTcommon.datadir := OpamFilename.Dir.of_string data_dir

let mk_opt ?section ?vopt flags value doc conv default =
  let doc = Arg.info ?docs:section ~docv:value ~doc flags in
  Arg.(value & opt ?vopt conv default & doc)

let repo_kinds = [
  "http" , `http;
  "local", `rsync;
  "git"  , `git;
  "darcs", `darcs;
  "hg"   , `hg;
]

let repo_kind_flag =
  let kinds =
    (* main kinds *)
    repo_kinds @ [
    (* aliases *)
    "wget" , `http;
    "curl" , `http;
    "rsync", `rsync;
  ] in
  mk_opt ["k";"kind"]
    "KIND" "Specify the kind of the repository to be set (the main ones \
            are 'http', 'local', 'git', 'darcs' or 'hg')."
    Arg.(some (enum kinds)) None

(* INIT *)
let init_doc = "Initialize an opam-rt instance."
let init =
  let doc = init_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,init) command creates a fresh opam-rt instance."
  ] in
  let path =
    let doc = Arg.info ~docv:"PATH" ~doc:"The local repository root." [] in
    Arg.(required & pos 0 (some dirname) None & doc) in
  let init seed data kind path test =
    set_seed seed;
    apply_data_dir data;
    let module Test = (val test: OpamRT.TEST) in
    try Test.init kind path with OpamRT.Not_available -> () in
  Term.(pure init $seed_flag $data_dir $repo_kind_flag $path $test_case),
  term_info "init" ~doc ~man

let run_test test kind path =
  let module Test = (val test: OpamRT.TEST) in
  let result =
    try Test.run kind path; `Ok with
    | Sys.Break -> prerr_endline "[interrupted]"; `No_result
    | OpamRT.Not_available -> `Skipped
    | OpamRT.Allowed_failure -> `Allowed_fail
    | OpamStd.Sys.Exit 0 -> `Ok
    | e -> `Failed
  in
  let result_file = OpamFilename.of_string "results" in
  let current =
    if OpamFilename.exists result_file then
      OpamStd.String.split (OpamFilename.read result_file) '\n'
    else []  in
  let opam_version =
    match OpamSystem.read_command_output ["opam"; "--git-version"] with
    | [v] -> v
    | _ -> "none"
  in
  let current = match current with
    | title::results when title = opam_version -> List.rev results
    | _ -> []
  in
  let title = Printf.sprintf "%-12s / %-5s / %-8s :" Test.name
      (match kind with
       | Some k -> List.assoc k (List.map (fun (a,b) -> b,a) repo_kinds)
       | None -> "none")
      (match OpamSolverConfig.external_solver_command
               ~input:"$in" ~output:"$out" ~criteria:"$criteria"
       with
       | Some s -> String.concat " " s
       | None -> "internal")
  in
  let current =
    List.filter
      (fun s -> not (OpamStd.String.starts_with ~prefix:title s))
      current
  in
  let results = match result with
    | `Ok -> Printf.sprintf "%s\t%s" title "OK" :: current
    | `Skipped -> Printf.sprintf "%s\t%s" title "SKIP" :: current
    | `Failed -> Printf.sprintf "%s\t%s" title "FAIL" :: current
    | `Allowed_fail -> Printf.sprintf "%s\t%s" title "FAILOK" :: current
    | `No_result -> current
  in
  OpamFilename.write result_file
    (String.concat "\n" (opam_version::(List.rev results)) ^ "\n");
  if result = `Failed then OpamStd.Sys.exit 1

(* RUN *)
let run_doc = "Run a given test suite."
let run =
  let doc = run_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,test) command runs the given opam-rt test suite."
  ] in
  let path =
    let doc = Arg.info ~docv:"PATH" ~doc:"The local repository root." [] in
    Arg.(required & pos 0 (some dirname) None & doc) in
  let run seed data kind path test =
    set_seed seed;
    apply_data_dir data;
    run_test test kind path in
  Term.(pure run $seed_flag $data_dir $repo_kind_flag $path $test_case),
  term_info "run" ~doc ~man

(* TEST *)
let test_doc = "Init and run a given test suite."
let test =
  let doc = test_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,test) command inits and runs the given opam-rt test suite."
  ] in
  let path =
    let doc = Arg.info ~docv:"PATH" ~doc:"The local repository root." [] in
    Arg.(required & pos 0 (some dirname) None & doc) in
  let test seed data kind path test =
    apply_data_dir data;
    set_seed seed;
    let module Test = (val test: OpamRT.TEST) in
    if OpamFilename.exists_dir path
    && OpamConsole.confirm "Do you want to remove %s ?" (OpamFilename.Dir.to_string path)
    then OpamFilename.rmdir path;
    (try Test.init kind path with OpamRT.Not_available -> ());
    run_test test kind path in
  Term.(pure test $seed_flag $data_dir $repo_kind_flag $path $test_case),
  term_info "test" ~doc ~man

(* LIST *)
let list_doc = "List available test suites."
let list =
  let doc = list_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,list) command lists the names of available test suites on \
        standard output";
  ] in
  let list () =
    print_endline (String.concat " " (List.map fst OpamRT.tests))
  in
  Term.(pure list $ pure ()),
  term_info "list" ~doc ~man

let default =
  let doc = "Regression Testing Framework for OPAM" in
  let man = [
    `P "Use either $(b,opam-rt <command> --help) or $(b,opam-rt help <command>) \
        for more information on a specific command.";
  ] @  help_sections
  in
  let usage () =
    OpamConsole.msg
      "usage: opam-rt [--version]\n\
      \               [--help]\n\
      \               <command> [<args>]\n\
       \n\
       The most commonly used opam commands are:\n\
      \    init         %s\n\
      \    run          %s\n\
      \    test         %s\n\
       \n\
       See 'opam-rt help <command>' for more information on a specific command.\n"
      init_doc run_doc test_doc in
  Term.(pure usage $ pure ()),
  Term.info "opam-rt"
    ~version
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = [
  init;
  run;
  list;
  test;
]

let _ =
  (* Installing files respects umask, which may then create a diff *)
  Unix.umask 0o022

let () =
  OpamClientConfig.opam_init ();
  try
    match Term.eval_choice ~catch:false default commands with
    | `Error _ -> exit 1
    | _        -> exit 0
  with
  | OpamStd.Sys.Exit 0 -> ()
  | OpamStd.Sys.Exec (cmd,args,env) ->
    Unix.execvpe cmd args env
  | e                  ->
    if OpamConsole.verbose () then
      Printf.eprintf "'%s' failed.\n" (String.concat " " (Array.to_list Sys.argv));
    let exit_code = ref 1 in
    begin match e with
      | OpamStd.Sys.Exit i ->
        exit_code := i;
        if OpamConsole.debug () && i <> 0 then
          Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e)
      | OpamSystem.Internal_error _
      | OpamSystem.Process_error _ ->
        Printf.eprintf "%s\n" (Printexc.to_string e);
        Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e);
      | Sys.Break -> exit_code := 130
      | Failure msg ->
        Printf.eprintf "Fatal error: %s\n" msg;
        Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e);
      | _ ->
        Printf.eprintf "Fatal error:\n%s\n" (Printexc.to_string e);
        Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e);
    end;
    exit !exit_code

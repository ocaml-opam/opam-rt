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

open OpamTypes
open Cmdliner
open OpamArg

let version = "0.0.1"

(* Help sections common to all commands *)
let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";

  `S "FURTHER DOCUMENTATION";
  `P (Printf.sprintf "See %s." OpamGlobals.default_repository_address);

  `S "AUTHOR";
  `P "Thomas Gazagnaire   <thomas.gazagnaire@ocamlpro.com>"; `Noblank;
]

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
  let init global_options seed kind path test =
    apply_global_options global_options;
    set_seed seed;
    let module Test = (val test: OpamRT.TEST) in
    Test.init kind path in
  Term.(pure init $global_options $seed_flag $repo_kind_flag $path $test_case),
  term_info "init" ~doc ~man

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
  let run global_options seed kind path test =
    apply_global_options global_options;
    set_seed seed;
    let module Test = (val test: OpamRT.TEST) in
    Test.run path in
  Term.(pure run $global_options $seed_flag $repo_kind_flag $path $test_case),
  term_info "run" ~doc ~man

(* LIST *)
let list_doc = "List available test suites."
let list =
  let doc = list_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,list) command lists the names of available test suites on \
        standard output";
  ] in
  let list global_options =
    print_endline (String.concat " " (List.map fst OpamRT.tests))
  in
  Term.(pure list $ global_options),
  term_info "list" ~doc ~man

let default =
  let doc = "Regression Testing Framework for OPAM" in
  let man = [
    `P "Use either $(b,opam-rt <command> --help) or $(b,opam-rt help <command>) \
        for more information on a specific command.";
  ] @  help_sections
  in
  let usage global_options =
    apply_global_options global_options;
    OpamGlobals.msg
      "usage: opam-rt [--version]\n\
      \               [--help]\n\
      \               <command> [<args>]\n\
       \n\
       The most commonly used opam commands are:\n\
      \    init         %s\n\
      \    run          %s\n\
       \n\
       See 'opam-rt help <command>' for more information on a specific command.\n"
      init_doc run_doc in
  Term.(pure usage $global_options),
  Term.info "opam-rt"
    ~version
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = [
  init;
  run;
  list;
]

let () =
  OpamArg.run default commands

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

val seed_ref : int ref
val set_seed : int -> unit
val seed : unit -> int
val datadir : dirname ref
val data : string -> filename
val system_switch : switch

module Color : sig
  val red : ('a, unit, string, string) format4 -> 'a
  val green : ('a, unit, string, string) format4 -> 'a
  val yellow : ('a, unit, string, string) format4 -> 'a
  val blue : ('a, unit, string, string) format4 -> 'a
end

module Git : sig
  val commit : dirname -> ('a, unit, string, unit) format4 -> 'a
  val commit_file :
    dirname -> filename -> ('a, unit, string, unit) format4 -> 'a
  val commit_dir :
    dirname -> dirname -> ('a, unit, string, unit) format4 -> 'a
  val revision : dirname -> string
  val commits : dirname -> string list
  val init : dirname -> unit
  val test_tag : string
  val branch : dirname -> unit
  val master : dirname -> unit
  val add : dirname -> filename -> unit
  val add_list : dirname -> filename list -> unit
  val checkout : dirname -> string -> unit
  val msg :
    dirname -> string -> package -> ('a, unit, string, unit) format4 -> 'a
end

val random_string : int -> string
val base : string -> basename

module Contents : sig
  val log :
    ?level:int ->
    (string -> unit, out_channel, unit) format -> string -> unit
  type t = (basename * string) list
  val files : int -> (basename * string * int) list
  val install : name -> basename * string * int
  val create : package -> int -> (basename * string * int) list
  val read : dirname -> package -> (basename * string * int) list
  val write : dirname -> package -> (basename * string * int) list -> unit
end

module Packages : sig
  val log :
    ?level:int ->
    (string -> unit, out_channel, unit) format -> string -> unit
  type t = {
    nv : package;
    prefix : string option;
    opam : OpamFile.OPAM.t;
    files : (basename * string * int) list;
    contents : (basename * string * int) list;
    archive : string option;
  }
  val opam : package -> [> `git | `rsync ] option -> dirname -> int -> OpamFile.OPAM.t
  val mandatory_fields: string -> ?seed:int -> OpamFile.OPAM.t -> OpamFile.OPAM.t
  val add_depend :
    t -> ?formula:(relop * filter) generic_formula -> string -> t
  val add_depend_with_runtime_checks :
    dirname -> t -> ?formula:(relop * filter) generic_formula -> string -> t
  val archive :
    (basename * string * int) list -> package -> int -> string option
  val prefix : package -> string option
  val files : int -> (basename * string * int) list
  val write_o : ('a -> unit) -> 'a option -> unit
  val write : dirname -> dirname -> t -> unit
  val read_o : (filename -> 'a) -> filename -> 'a option
  val read : dirname -> dirname -> string option -> package -> t
  val add : dirname -> dirname -> t -> unit
end

module OPAM : sig
  val opam :
    ?fake:bool ->
    ?env:string list -> dirname -> string -> string list -> unit
  val var : dirname -> string -> string
  val init : dirname -> repository_name -> url -> unit
  val install : dirname -> ?version:version -> name -> unit
  val install_code : dirname -> ?version:version -> name -> int
  val install_dir : dirname -> ?recs:bool -> ?subpath:string -> dirname -> unit
  val reinstall : dirname -> ?version:version -> name -> unit
  val remove : dirname -> ?auto:bool -> name -> unit
  val update : dirname -> unit
  val upgrade : dirname -> ?fake:bool -> name list -> unit
  val pin : dirname -> ?action:bool -> name -> dirname -> unit
  val vpin : dirname -> name -> version -> unit
  val pin_kind :
    dirname -> ?action:bool -> ?kind:string -> name -> string -> unit
  val unpin : dirname -> ?action:bool -> name -> unit
  val pin_edit : dirname -> ?action:bool -> name -> (filename -> 'a) -> unit
  val import : dirname -> ?fake:bool -> filename -> unit
  val export : dirname -> filename -> unit
end

val repo_opams : dirname -> OpamFile.OPAM.t package_map

module Check :
sig
  module A = OpamFilename.Attribute
  type error = { source : string; attr : file_attribute; file : filename; }
  exception Sync_errors of error list
  val sync_errors : error list -> 'a
  val set : 'a A.Map.t -> file_attribute_set
  exception Found of file_attribute * filename
  val find_binding :
    (file_attribute -> filename -> bool) ->
    filename A.Map.t -> file_attribute * filename
  val attributes :
    ?filter:(filename -> dirname option) -> dirname -> filename A.Map.t
  val sym_diff :
    string * filename A.Map.t -> string * filename A.Map.t -> error list
  val check_attributes :
    string * filename A.Map.t -> string * filename A.Map.t -> unit
  val check_dirs :
    ?filter:(filename -> dirname option) ->
    string * dirname -> string * dirname -> unit
  val installed : dirname -> package_set
  val package_of_filename : filename -> dirname * package
  val check_invariants : dirname -> unit
  val packages : dirname -> dirname -> unit
  val contents : dirname -> package -> OpamFile.OPAM.t -> unit
end

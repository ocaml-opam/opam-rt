## opam-rt

The goal of opam-rt is to test for regressions in OPAM.

OPAM has already its own [test
suite](https://github.com/OCamlPro/opam/tree/master/tests) but some
corner-cases are more easily tested in a more dedicated framework
(ie. not a collection of shell scripts, but something well-designed
and written in a real programming language).

### Status

`opam-rt` currently test mainly two areas of OPAM:

* pinned packages
* development packages

### Compilation

Make sure you have the required dependencies installed (using `opam`):

* `cohttp-lwt-unix`
* `opam-client`

Any external contribution, as testing ideas or pull requests are most
 than welcome.

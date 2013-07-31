## opam-rt

The goal of opam-rt is to test for regressions in OPAM.

OPAM has already its own [test
suite](https://github.com/OCamlPro/opam/tree/master/tests) but some
corner-cases are more easily tested in a more dedicated framework
(ie. not a collection of shell scripts, but something well-designed
and written in a real programming language).

  ### Status

This is work-in-progress, mainly dedicated to test the new `opam
update` strategy (see https://github.com/OCamlPro/opam/pull/719). As
it is still the early days, the API will evolve quite quickly, but any
help to write some regression tests will be greatly appreciated.
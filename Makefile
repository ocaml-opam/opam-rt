BUILD=ocamlbuild -use-ocamlfind -pkgs opam.client,cohttp.lwt -no-links -cflags -bin-annot
TARGETS=src/opamRTmain.native src/file_server.native
OPAMRT=./opam-rt
TESTDIR=/tmp/xxx
KINDS = local http git

.PHONY: all opam-rt run

all: opam-rt

opam-rt:
	$(BUILD) $(TARGETS)
	ln -f _build/src/opamRTmain.native opam-rt
	ln -f _build/src/file_server.native file-server

run: opam-rt
	@for kind in $(KINDS); do \
	  for test in $(shell $(OPAMRT) list); do \
	    rm -rf $(TESTDIR) && \
	    ( echo "TEST:" $$test-$$kind && \
	      $(OPAMRT) init $(TESTDIR) $$test --kind $$kind && \
	      $(OPAMRT) run  $(TESTDIR) $$test --kind $$kind ) \
	    || exit; \
	  done; \
	done

clean:
	rm -rf _build opam-rt file-server $(TESTDIR)

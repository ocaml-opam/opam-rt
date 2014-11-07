BUILD=ocamlbuild -use-ocamlfind -pkgs opam-lib.client,cohttp.lwt -no-links -cflags -bin-annot
TARGETS=src/opamRTmain.native src/file_server.native
OPAMRT=OPAMYES=1 ./opam-rt
TESTDIR=/tmp/xxx
KINDS = local http git

.PHONY: all opam-rt run

all: opam-rt

opam-rt:
	$(BUILD) $(TARGETS)
	ln -f _build/src/opamRTmain.native opam-rt
	ln -f _build/src/file_server.native file-server

show_results = @sed -e 's/\tOK/\t[32mOK[m/' -e 's/\tFAILOK/\t[33mFAILOK[m/' -e 's/\tFAIL/\t[31mFAIL[m/' results


.PHONY: $(KINDS)
$(KINDS):
	for test in $(shell $(OPAMRT) list); do \
	  ( echo "TEST:" $$test-$@ && \
	    $(OPAMRT) test $(TESTDIR) $$test --kind $@) \
	  || true; \
	done;
	@echo
	$(show_results)

run: opam-rt
	@for kind in $(KINDS); do \
	  for test in $(shell $(OPAMRT) list); do \
	    ( echo "TEST:" $$test-$$kind && \
	      $(OPAMRT) test $(TESTDIR) $$test --kind $$kind) \
	    || true; \
	  done; \
	done;
	@echo
	@echo "================ RESULTS ==============="
	$(show_results)

clean:
	rm -rf _build opam-rt file-server $(TESTDIR)

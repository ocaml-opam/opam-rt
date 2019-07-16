OPAMRT=OPAMYES=1 OPAMSWITCH= ./opam-rt
TESTDIR=/tmp/opam-rt
KINDS = local http git

.PHONY: all opam-rt run

all: opam-rt

opam-rt:
	dune build src/lib/opam_rt_lib.cma src/tests/main.exe src/server/server.exe
	ln -f _build/default/src/tests/main.exe opam-rt
	ln -f _build/default/src/server/server.exe opam-rt-server

show_results = @sed -e 's/\tOK/\t[32mOK[m/' -e 's/\tFAILOK/\t[33mFAILOK[m/' -e 's/\tFAIL/\t[31mFAIL[m/' results


.PHONY: $(KINDS)
$(KINDS):
	@rm -f failed
	@for test in $(shell $(OPAMRT) list); do \
	  ( echo "TEST:" $$test-$@ && \
	    $(OPAMRT) test $(TESTDIR) $$test --kind $@) \
	  || touch failed; \
	done;
	@echo
	$(show_results)
	@if [ -e failed ]; then rm failed; false; fi

run:
	@rm -f failed
	@for kind in $(KINDS); do \
	  for test in $(shell $(OPAMRT) list); do \
	    ( echo "TEST:" $$test-$$kind && \
	      $(OPAMRT) test $(TESTDIR) $$test --kind $$kind) \
	    || touch failed; \
	  done; \
	done;
	@echo
	@echo "================ RESULTS ==============="
	$(show_results)
	@if [ -e failed ]; then rm failed; false; fi

clean:
	rm -rf _build opam-rt opam-rt-server $(TESTDIR)

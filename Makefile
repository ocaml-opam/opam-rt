BUILD=ocamlbuild -use-ocamlfind -pkgs opam.client,cohttp.lwt -no-links
TARGETS=src/opamRTmain.native src/file_server.native

OPAMRT=./opam-rt
TESTDIR=/tmp/xxx

all: opam-rt

.PHONY: opam-rt
opam-rt:
	$(BUILD) $(TARGETS)
	ln -f _build/src/opamRTmain.native opam-rt
	ln -f _build/src/file_server.native file-server

TESTS = $(shell $(OPAMRT) list)
KINDS = local http git
ALLTESTS = $(foreach test,$(TESTS),$(foreach kind,$(KINDS),$(test).$(kind)))

$(ALLTESTS): all
	rm -rf $(TESTDIR)
	$(OPAMRT) init $(TESTDIR) $(basename $@) --kind $(@:$(basename $@).%=%)
	$(OPAMRT) run  $(TESTDIR) $(basename $@)

.PHONY: $(TESTS)
$(TESTS):
	$(MAKE) $(foreach kind,$(KINDS),$@.$(kind))

.PHONY: $(KINDS)
$(KINDS):
	$(MAKE) $(foreach test,$(TESTS),$(test).$@)

dev-update.http pin-update.http pin-install.http:
	@echo -e "############## $@: TODO ##############"

.PHONY: run
run: $(TESTS)

clean:
	rm -rf _build opam-rt file-server $(TESTDIR)

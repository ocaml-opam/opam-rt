BUILD=ocamlbuild -use-ocamlfind -pkgs opam.client,cohttp.lwt -no-links
TARGETS=src/opamRTmain.native src/file_server.native

OPAMRT=./opam-rt
TESTDIR=/tmp/xxx

all:
	$(BUILD) $(TARGETS)
	ln -f _build/src/opamRTmain.native opam-rt
	ln -f _build/src/file_server.native file-server

repo-http:
	rm -rf $(TESTDIR)
	$(OPAMRT) init $(TESTDIR) repo-update --kind http
	$(OPAMRT) run $(TESTDIR)  repo-update

repo-local:
	rm -rf $(TESTDIR)
	$(OPAMRT) init $(TESTDIR) repo-update --kind local
	$(OPAMRT) run $(TESTDIR)  repo-update

repo-git:
	rm -rf $(TESTDIR)
	$(OPAMRT) init $(TESTDIR) repo-update --kind git
	$(OPAMRT) run $(TESTDIR)  repo-update

dev-local:
	rm -rf $(TESTDIR)
	$(OPAMRT) init $(TESTDIR) dev-update --kind local
	$(OPAMRT) run $(TESTDIR)  dev-update

dev-git:
	rm -rf $(TESTDIR)
	$(OPAMRT) init $(TESTDIR) dev-update --kind git
	$(OPAMRT) run $(TESTDIR)  dev-update

pin-local:
	rm -rf $(TESTDIR)
	$(OPAMRT) init $(TESTDIR) pin-update --kind local
	$(OPAMRT) run $(TESTDIR)  pin-update

pin-git:
	rm -rf $(TESTDIR)
	$(OPAMRT) init $(TESTDIR) pin-update --kind git
	$(OPAMRT) run $(TESTDIR)  pin-update

run:
	$(MAKE) repo-local
	$(MAKE) repo-git
	$(MAKE) repo-http
	$(MAKE) dev-local
	$(MAKE) dev-git
	$(MAKE) pin-local
	$(MAKE) pin-git

clean:
	rm -rf _build opam-rt file-server $(TESTDIR)

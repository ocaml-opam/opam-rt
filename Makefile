BUILD=ocamlbuild -use-ocamlfind -pkgs opam.client -no-links
TARGET=src/opamRTmain.native

OPAMRT=./opam-RT
TESTDIR=/tmp/xxx

all:
	$(BUILD) $(TARGET)
	ln -f _build/$(TARGET) opam-rt

repo-local:
	rm -rf $(TESTDIR)
	$(OPAMRT) init $(TESTDIR) repo-update --kind local
	$(OPAMRT) run $(TESTDIR)  repo-update --kind local

repo-git:
	rm -rf $(TESTDIR)
	$(OPAMRT) init $(TESTDIR) repo-update --kind git
	$(OPAMRT) run $(TESTDIR)  repo-update --kind git

dev-local:
	rm -rf $(TESTDIR)
	$(OPAMRT) init $(TESTDIR) dev-update --kind local
	$(OPAMRT) run $(TESTDIR)  dev-update --kind local

dev-git:
	rm -rf $(TESTDIR)
	$(OPAMRT) init $(TESTDIR) dev-update --kind git
	$(OPAMRT) run $(TESTDIR)  dev-update --kind git

run:
	$(MAKE) repo-local
	$(MAKE) repo-git
	$(MAKE) dev-local
	$(MAKE) dev-git

clean:
	rm -rf _build opam-rt $(TESTDIR)

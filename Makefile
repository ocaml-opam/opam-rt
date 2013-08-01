BUILD=ocamlbuild -use-ocamlfind -pkgs opam.client -no-links
TARGET=src/opamRTmain.native

OPAMRT=./opam-RT
TESTDIR=/tmp/xxx

all:
	$(BUILD) $(TARGET)
	ln -f _build/$(TARGET) opam-rt

base-rsync:
	rm -rf $(TESTDIR)
	$(OPAMRT) init $(TESTDIR) base --kind local
	$(OPAMRT) run $(TESTDIR)  base --kind local

base-git:
	rm -rf $(TESTDIR)
	$(OPAMRT) init $(TESTDIR) base --kind git
	$(OPAMRT) run $(TESTDIR)  base --kind git

run:
	$(MAKE) base-rsync
	$(MAKE) base-git

clean:
	rm -rf _build opam-rt $(TESTDIR)

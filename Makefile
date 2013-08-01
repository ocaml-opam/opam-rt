BUILD=ocamlbuild -use-ocamlfind -pkgs opam.client -no-links
TARGET=src/opamRTmain.native

OPAMRT=./opam-RT
TMPDIR=/tmp/xxx

all:
	$(BUILD) $(TARGET)
	ln -f _build/$(TARGET) opam-rt

base-rsync:
	rm -rf $(TMPDIR)
	$(OPAMRT) init $(TMPDIR) base --kind local
	$(OPAMRT) run $(TMPDIR)  base --kind local

base-git:
	rm -rf $(TMPDIR)
	$(OPAMRT) init $(TMPDIR) base --kind git
	$(OPAMRT) run $(TMPDIR)  base --kind git

run:
	$(MAKE) base-rsync
	$(MAKE) base-git

clean:
	rm -rf _build opam-rt $(TMPDIR)

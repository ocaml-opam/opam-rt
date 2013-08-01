BUILD=ocamlbuild -use-ocamlfind -pkgs opam.client -no-links
TARGET=src/opamRTmain.native

OPAMRT=./opam-RT
TMPDIR=/tmp/xxx

all:
	$(BUILD) $(TARGET)
	ln -f _build/$(TARGET) opam-rt

run:
	rm -rf $(TMPDIR)
	$(OPAMRT) init $(TMPDIR) base
	$(OPAMRT) run $(TMPDIR)  base

clean:
	rm -rf _build opam-rt $(TMPDIR)

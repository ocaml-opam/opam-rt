BUILD=ocamlbuild -use-ocamlfind -pkgs opam.client -no-links
TARGET=src/opamRTmain.native

all:
	$(BUILD) $(TARGET)
	ln -f _build/$(TARGET) opam-rt

clean:
	rm -rf _build opam-rt

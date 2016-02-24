PREFIX=$(shell opam config var prefix)

all: opam-depext

ALWAYS:

opam-depext: _build/depext.native ALWAYS
	cp $< $@

_build/%: ALWAYS
	ocamlbuild -tags annot,bin_annot,debug,use_unix -pkg cmdliner $*

clean:
	rm -rf _build opam-depext depext.native

install:
	opam-installer --prefix=$(PREFIX)

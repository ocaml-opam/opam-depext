all: opam-depext

ALWAYS:

opam-depext: _build/depext.native ALWAYS
	cp $< $@

_build/%: ALWAYS
	ocamlbuild -tags debug,use_unix -pkg cmdliner $*

clean:
	rm -rf _build opam-depext

all: opam-depext

opam-depext: _build/depext.native
	cp $^ $@

_build/%:
	ocamlbuild -tags debug,use_unix $*

clean:
	rm -rf _build opam-depext

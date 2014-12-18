all: opam-installext

opam-installext: _build/installext.native
	cp $^ $@

_build/%:
	ocamlbuild -tags debug,use_unix $*

clean:
	rm -rf _build opam-installext

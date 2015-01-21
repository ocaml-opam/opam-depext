OPAM depext plugin

Replacement for the currently used shell-scripts handling distribution-specific
installation of OPAM packages' external dependencies (as per the `depexts` field
in their definitions).

This is a first prototype, largely incomplete ; it may help pave the way to some
specification of the `depexts` field -- and may give some basis for a real
plugin integrating with OPAM (see
https://github.com/ocaml/opam/blob/master/doc/design/depexts-plugins)

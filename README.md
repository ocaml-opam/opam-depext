OPAM depext plugin
==================

Replacement for the currently used shell-scripts handling distribution-specific
installation of OPAM packages' external dependencies (as per the `depexts` field
in their definitions).

Currently supported depexts are:

* `homebrew` `osx`
* `macports` `osx`
* `debian` `linux`
* `ubuntu` `linux`
* `centos` `linux`
* `fedora` `linux`
* `rhel` `linux`
* `oraclelinux` `linux`
* `mageia` `linux`
* `alpine` `linux`
* `archlinux` `linux`
* `opensuse` `linux`
* `win32` `cygwin`
* `gentoo`
* `freebsd`
* `openbsd`
* `netbsd`
* `dragonfly`
 
This version runs as an OPAM plugin; it may help pave the way to some
specification of the `depexts` field and give some basis for a real
plugin integrating with OPAM (see
https://github.com/ocaml/opam/blob/master/doc/design/depexts-plugins)

Questions: email <opam-devel@lists.ocaml.org>

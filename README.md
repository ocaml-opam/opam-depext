OPAM depext plugin
==================

Replacement for the currently used shell-scripts handling distribution-specific
installation of OPAM packages' external dependencies (as per the `depexts` field
in their definitions).

Currently supported depexts are:

| Depext                 | Online package list                                     |
|------------------------|---------------------------------------------------------|
| `alpine` `linux`       | https://pkgs.alpinelinux.org                            |
| `archlinux` `linux`    | https://www.archlinux.org/packages/                     |
| `centos` `linux`       | http://centos-packages.com/                             |
| `debian` `linux`       | https://www.debian.org/distrib/packages#search_packages |
| `dragonfly`            |                                                         |
| `fedora` `linux`       | https://apps.fedoraproject.org/packages/                |
| `freebsd`              | https://www.freebsd.org/ports/index.html                |
| `gentoo`               | https://packages.gentoo.org/                            |
| `homebrew` `osx`       | http://braumeister.org/                                 |
| `macports` `osx`       | https://www.macports.org/ports.php                      |
| `mageia` `linux`       | https://madb.mageia.org/                                |
| `netbsd`               | http://pkgsrc.se/                                       |
| `openbsd`              | http://openports.se/                                    |
| `opensuse` `linux`     | https://software.opensuse.org/find                      |
| `oraclelinux` `linux`  |                                                         |
| `rhel` `linux`         |                                                         |
| `ubuntu` `linux`       | https://packages.ubuntu.com/                            |
| `win32` `cygwin`       |                                                         |
 
This version runs as an OPAM plugin; it may help pave the way to some
specification of the `depexts` field and give some basis for a real
plugin integrating with OPAM (see
https://github.com/ocaml/opam/blob/master/doc/design/depexts-plugins)

Questions: email <opam-devel@lists.ocaml.org>

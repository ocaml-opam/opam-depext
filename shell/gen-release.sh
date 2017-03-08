#!/bin/sh
# Generate a release of a particular tag of opam-depext with
# the vendor tarballs included.

if [ "$1" = "" ]; then
  echo Usage: $0 version
  exit 1
fi

git archive --format=tar --prefix=opam-depext-$1/ master > ../opam-depext-$1.tar v$1
cd ..
tar -xvf opam-depext-$1.tar
cd opam-depext-$1
make distrib
cd ..
tar -jcvf opam-depext-full-$1.tbz opam-depext-$1
rm -f opam-depext-$1.tar

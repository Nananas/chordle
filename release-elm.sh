#!/bin/bash -ex

make -C elm
make -C elm optimize
make -C elm minify
make -C elm archive
./upload-elm.sh elm/chordle.tar.gz 
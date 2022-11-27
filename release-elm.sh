#!/bin/bash -ex

sed 's/--.*//g' words.json > static/words.json

make -C elm
make -C elm optimize
make -C elm minify
make -C elm archive
./upload-elm.sh elm/chordle.tar.gz 
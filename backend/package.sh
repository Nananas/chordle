#!/bin/bash

cargo build --release

rm -rf docker/build
mkdir -p docker/build

cp target/x86_64-unknown-linux-musl/release/chordle-backend docker/build
cp Rocket.toml docker/
(cd docker && docker build --tag chordle-backend -f Dockerfile .)
docker save chordle-backend:latest | gzip > chordle-backend.tar.gz

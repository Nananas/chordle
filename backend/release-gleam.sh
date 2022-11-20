#!/bin/bash -ex

gleam build
gleam export erlang-shipment

docker build --tag chordle-server -f Dockerfile build
docker save chordle-server:latest | gzip > chordle-server.tar.gz

./upload-gleam.sh
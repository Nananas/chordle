# This docker image (and by extension the container) is essentially just packaging
# the required files, with the addition of access to the docker network (psql db container)


# docker build --tag chordle-server -f Dockerfile .
# docker save chordle-server:latest | gzip > chordle-server.tar.gz

FROM alpine

# Add project code
COPY build/chordle-backend /stage/
COPY Rocket.toml /stage/

# Run the server
WORKDIR /stage
ENTRYPOINT ["/stage/chordle-backend"]

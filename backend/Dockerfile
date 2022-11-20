FROM erlang:25-alpine

# Add project code
COPY erlang-shipment /stage/server

# Run the server
WORKDIR /stage
ENTRYPOINT ["/stage/server/entrypoint.sh"]
CMD ["run"]

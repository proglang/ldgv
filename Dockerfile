FROM haskell:8.10
WORKDIR /opt/example

COPY ./stack.yaml ./stack.yaml.lock ./ld-session-code.cabal /opt/example
RUN stack setup
RUN stack build --only-dependencies --test --no-run-tests

COPY . /opt/example
RUN stack test
RUN stack install
ENTRYPOINT ["/root/.local/bin/ld-session-code-exe"]

## FROM alpine:latest
## WORKDIR /root
## copy --from=0 /root/.local/bin/ld-session-code-exe .
## CMD ["./ld-session-code-exe"]


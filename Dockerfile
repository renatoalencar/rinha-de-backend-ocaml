FROM ocaml/opam

WORKDIR /app
COPY backend.opam backend.opam
RUN opam install . --unlock-base -y --deps-only
COPY . .
RUN dune build
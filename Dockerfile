FROM ocaml/opam2:ubuntu-16.04-ocaml-4.06

RUN opam update
RUN eval `opam config env`
RUN opam install dune

RUN sudo apt-get update
RUN sudo apt-get install -y pkg-config libcurl4-gnutls-dev m4 protobuf-compiler

RUN mkdir geneweb
RUN mkdir bases

WORKDIR geneweb
COPY geneweb.opam .

RUN opam pin add geneweb . --no-action -y
RUN opam install geneweb --deps-only -y

EXPOSE 2317
EXPOSE 2316

WORKDIR /home/opam/geneweb/docker/

CMD bash ./build.sh

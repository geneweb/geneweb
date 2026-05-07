# syntax=docker/dockerfile:1
# ================================================
# STAGE 1: build binaries with embedded libraries
# ================================================
FROM ocaml/opam:debian-12-ocaml-5.4 AS bundle-builder
USER root

RUN apt-get update
RUN apt-get install -yy \
  brotli \
  file \
  libbrotli-dev \
  libgmp-dev \
  libpcre2-dev \
  m4 \
  makeself \
  patchelf \
  pkg-config \
  xdot

# Workaround
RUN mkdir -p /lib64

USER opam
WORKDIR /home/opam
ENV OPAMYES=yes

COPY --chown=opam:opam *.opam Makefile .
RUN OPAM=opam-2.5 make deps

COPY --chown=opam:opam . .
RUN opam-2.5 exec -- make bundle

RUN yes | ./geneweb-*.run -- --prefix /home/opam/bundle

# ================================================
# STAGE 2: build user environment
# ================================================
FROM busybox:latest AS user-builder

RUN addgroup -g 10001 www
RUN adduser -D -u 10001 -G www www

# ================================================
# STAGE 3: export the image
# ================================================
FROM scratch AS exporter

COPY --from=bundle-builder /home/opam/bundle/geneweb /geneweb
COPY --from=bundle-builder /lib64/ld-linux-*.so.* /lib64/
COPY --from=bundle-builder /lib/ld-linux-*.so.* /lib/
COPY --from=bundle-builder /lib/*-linux-gnu/libm.so* /lib/
COPY --from=bundle-builder /lib/*-linux-gnu/libc.so* /lib/
COPY --from=user-builder --parents /etc/passwd /
COPY --from=user-builder --parents /etc/group /

# ================================================
# STAGE 3.a: serve gwsetup application
# ================================================
FROM exporter AS gwsetup

USER www
EXPOSE 2316

ENV GW_BASES_DIR=/bases
VOLUME [ "${GW_BASES_DIR}" ]

ENTRYPOINT ["/geneweb/bin/gwsetup"]

# ================================================
# STAGE 3.b: serve gwd application
# ================================================
FROM exporter AS gwd

COPY --from=user-builder /home/www /home/www

USER www
EXPOSE 2317

ENV GW_BASES_DIR=/bases
VOLUME [ "${GW_BASES_DIR}" ]

ENTRYPOINT ["/geneweb/bin/gwd"]

###############################################################################
#                                                      STAGE 1: Build Geneweb
###############################################################################

FROM ocaml/opam:debian-ocaml-4.14-nnp AS builder

ENV OPAMYES=yes
USER root
# Ignore the apt warning here as apt-get does not allow wildcarding versions
# hadolint ignore=DL3027
RUN export DEBIAN_FRONTEND=noninteractive \
&& apt install -yq --no-install-recommends \
    m4=1.4.19-3 \
    libgmp-dev=2:6.2.1+dfsg1-1.1 \
    libpcre3-dev=2:8.39-15 \
    libipc-system-simple-perl=1.30-2 \
    xdot=1.2-3 \
    zlib1g-dev=1:1.2.13.dfsg-1 \
    pkg-config=1.8.1-1 \
&& ln -sf /usr/bin/opam-2.3 /usr/bin/opam

USER opam
# Install problematic packages on ARM64 first to isolate any issues
RUN eval "$(opam env)" && opam install \
    uri.4.4.0 yojson.2.2.2 zarith.1.14

# Install deps before cloning Geneweb to store them in a stable reusable cache
RUN eval "$(opam env)" && opam install \
    ancient.0.9.1 calendars.1.0.0 camlp-streams.5.0.1 camlzip.1.13 \
    cppo.1.8.0 jingoo.1.5.0 markup.1.0.3 ppx_import.1.11.0 ppx_blob.0.7.2 \
    stdlib-shims.0.3.0 unidecode.0.2.0 uucp.16.0.0 uunf.16.0.0 uutf.1.0.3 \
    camlp5.8.03.01 oUnit.2.2.7 syslog.2.0.2

# Clone repository and build Geneweb
WORKDIR /home/opam/geneweb
COPY --chown=opam:opam . .
RUN eval "$(opam env)" && ocaml ./configure.ml --sosa-zarith && make distrib

###############################################################################
#                                       STAGE 2: Export build via blank image
###############################################################################

FROM scratch AS export
COPY --from=builder /home/opam/geneweb/distribution /

###############################################################################
#                                              STAGE 3: Assemble Docker image
###############################################################################

FROM debian:12-slim AS container

ENV GENEWEB_HOME=/usr/local/share/geneweb
ENV GENEWEB_DATA_PATH=${GENEWEB_HOME}/share/data
ENV GWSETUP_IP=172.17.0.1

# Install runtime tools and add Geneweb user
# Ignore the apt warning here as apt-get does not allow wildcarding versions
# hadolint ignore=DL3027
RUN apt-get update -q \
  && apt install -qy --no-install-recommends sudo openssl \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && adduser --system --group --uid 1000 \
     --home ${GENEWEB_HOME} --shell /bin/bash geneweb \
  && usermod -aG sudo geneweb \
  && echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

# Add required directories and copy geneweb distribution
USER geneweb
WORKDIR ${GENEWEB_HOME}

# Create directory structure and configure
RUN mkdir -p bin etc log share/data share/dist \
  && echo "${GWSETUP_IP}" >> etc/gwsetup_only

# Copy application files
COPY --from=builder /home/opam/geneweb/distribution share/dist
COPY docker/geneweb-launch.sh bin/geneweb-launch.sh

# Configure container
EXPOSE 2316-2317
VOLUME [ "${GENEWEB_DATA_PATH}", "${GENEWEB_HOME}/etc" ]

CMD [ "bin/geneweb-launch.sh" ]

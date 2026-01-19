###############################################################################
#                                                      STAGE 1: Build Geneweb
###############################################################################

FROM ocaml/opam:debian-ocaml-4.14-nnp@sha256:6dd7a8b14d2d6dbdc5c0d7dcc5bb1bd1136d4b17abe12e9fece2464931bbf9f9 AS builder

ENV OPAMYES=yes
ENV OPAMJOBS=2
ENV DUNE_PROFILE=release

USER root
# Install required system dependencies
RUN export DEBIAN_FRONTEND=noninteractive \
    && apt-get update \
    && apt-get install -yq --no-install-recommends \
    m4 libgmp-dev libpcre2-dev libipc-system-simple-perl xdot zlib1g-dev pkg-config brotli \
    && ln -sf /usr/bin/opam-2.3 /usr/bin/opam

# Update local opam repository
USER opam
WORKDIR /home/opam/opam-repository
RUN git fetch origin master && git checkout 21f97b2e6d7c617fc0a79cc8e59b2e10690960e1 && opam update

# Initialize OPAM
WORKDIR /home/opam
RUN opam init --disable-sandboxing --auto-setup --bare

# Copy opam file for dependency resolution then install dependencies
COPY --chown=opam:opam *.opam ./
RUN eval "$(opam env)" && opam install . --deps-only --with-test && opam install ancient

# Clone repository and build Geneweb
WORKDIR /home/opam/geneweb
COPY --chown=opam:opam . .
RUN eval "$(opam env)" && ocaml ./configure.ml --sosa-zarith --gwd-caching && make distrib

###############################################################################
#                                       STAGE 2: Export build via blank image
###############################################################################

FROM scratch AS export
COPY --from=builder /home/opam/geneweb/distribution /

###############################################################################
#                                              STAGE 3: Assemble Docker image
###############################################################################

FROM debian:13-slim AS container

ENV GENEWEB_HOME=/usr/local/share/geneweb
ENV GENEWEB_DATA_PATH=${GENEWEB_HOME}/share/data
ENV GWSETUP_IP=172.17.0.1

# Install runtime tools and add Geneweb user
# Ignore the apt warning here as apt-get does not allow wildcarding versions
# hadolint ignore=DL3027
RUN apt-get update -q \
  && apt install -qy --no-install-recommends openssl adduser netcat-openbsd \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && adduser --system --group --uid 1000 \
     --home ${GENEWEB_HOME} --shell /bin/bash geneweb

# Do everything in the Geneweb home directory
WORKDIR ${GENEWEB_HOME}

# Create directory structure and configure
RUN mkdir -p bin etc log share/data share/dist \
  && echo "${GWSETUP_IP}" >> etc/gwsetup_only

# Copy application files
COPY --from=builder /home/opam/geneweb/distribution share/dist
COPY docker/geneweb-launch.sh bin/geneweb-launch.sh

# Make script executable, ensure log files exists and update ownership
RUN chmod +x bin/geneweb-launch.sh \
  && touch log/gwsetup.log \
  && touch log/gwd.log \
  && chown -R geneweb:geneweb .

# Switch to geneweb user
USER geneweb

# Configure container
EXPOSE 2316-2317
VOLUME [ "${GENEWEB_DATA_PATH}", "${GENEWEB_HOME}/etc" ]

CMD [ "bin/geneweb-launch.sh" ]

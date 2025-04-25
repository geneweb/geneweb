###############################################################################
#                                                      STAGE 1: Build Geneweb
###############################################################################

FROM ocaml/opam:debian-ocaml-4.14-nnp AS builder

ENV OPAMYES=yes
ENV OPAMJOBS=2

USER root
# Install required system dependencies
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

# Initialize OPAM
USER opam
WORKDIR /home/opam
RUN opam init --disable-sandboxing --auto-setup --bare

# Copy opam file for dependency resolution then install dependencies
COPY --chown=opam:opam *.opam ./
RUN eval "$(opam env)" && opam install . --deps-only --with-test && opam install ancient

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

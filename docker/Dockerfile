###############################################################################
#                                                      STAGE 1: Build geneweb
###############################################################################
FROM ocaml/opam:debian-11-ocaml-4.14 AS builder
ENV OPAMYES=yes

# Install required packages for build
USER root

# Ignore the apt warning here as apt-get does not allow wildcarding versions
# hadolint ignore=DL3027
RUN export DEBIAN_FRONTEND=noninteractive \
    && apt-get update -q \  
    && apt install -yq --no-install-recommends \
        libgmp-dev=2:6.* \
        libipc-system-simple-perl=1.* \
        libstring-shellquote-perl=1.* \
        perl=5.*

# Set up geneweb package and dependencies
USER opam
RUN git clone https://github.com/geneweb/geneweb.git ./geneweb \
    && opam init --disable-sandboxing && opam update
WORKDIR /home/opam/geneweb
RUN opam pin add geneweb.dev . --no-action && opam depext geneweb 

# Build geneweb
RUN opam install geneweb --deps-only \
    && eval "$(opam env)" \
    && ocaml ./configure.ml --release && make clean distrib

###############################################################################
#                                       STAGE 2: Export build via blank image
###############################################################################

FROM scratch AS export
COPY --from=builder /home/opam/geneweb/distribution /

###############################################################################
#                                              STAGE 3: Assemble Docker image
###############################################################################

FROM debian:bullseye-slim AS container

# Install runtime tools and add geneweb user
# Ignore the apt warning here as apt-get does not allow wildcarding versions
# hadolint ignore=DL3027
RUN apt-get update -q \
    && apt install -qy sudo=1.* openssl=1.* \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && adduser --system --group --uid 1000 --home /usr/local/share/geneweb --shell /bin/bash geneweb \
    && usermod -aG sudo geneweb \
    && echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

# Add required directories and copy geneweb distribution
USER geneweb
WORKDIR /usr/local/share/geneweb
RUN mkdir -p bin etc log share/data share/dist \
    && echo "172.17.0.1" >> etc/gwsetup_only
COPY --from=builder /home/opam/geneweb/distribution share/dist
COPY geneweb-launch.sh bin/geneweb-launch.sh

EXPOSE 2316-2317
VOLUME [ "/usr/local/share/geneweb/share/data", "/usr/local/share/geneweb/etc" ]

# Ignore the USER warning here - geneweb-launch.sh uses sudo to run the services as an unprivileged user
# hadolint ignore=DL3002
USER root
CMD [ "bin/geneweb-launch.sh" ]

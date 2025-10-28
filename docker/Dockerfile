###############################################################################
#                                                      STAGE 1: Build Geneweb
###############################################################################

FROM ocaml/opam:debian-ocaml-4.14-nnp@sha256:adb86241b87b40c074ff5dcdb172aa3b408209f1cf57c85db8a1350d56a4a650 AS builder

# Build arguments
ARG USE_PYTHON_BINARIES=false

ENV OPAMYES=yes
ENV OPAMJOBS=2
ENV DUNE_PROFILE=release

USER root
# Install required system dependencies (including Python for conditional builds)
RUN export DEBIAN_FRONTEND=noninteractive \
    && apt-get update \
    && apt-get install -yq --no-install-recommends \
    m4 libgmp-dev libpcre3-dev libipc-system-simple-perl xdot zlib1g-dev pkg-config \
    python3 python3-pip python3-venv \
    && ln -sf /usr/bin/opam-2.3 /usr/bin/opam

# Update local opam repository
USER opam
WORKDIR /home/opam/opam-repository
RUN git fetch origin master && git checkout 2ca36230be22748ea153057760524e23defe5fc7 && opam update

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

# Build OCaml version
RUN eval "$(opam env)" && ocaml ./configure.ml --sosa-zarith --gwd-caching && make distrib

# Conditionally replace binaries with Python versions if tests passed
RUN if [ "$USE_PYTHON_BINARIES" = "true" ]; then \
        echo "🐍 Using Python binaries (tests passed)"; \
        # Install Python dependencies \
        python3 -m venv /home/opam/python-env; \
        . /home/opam/python-env/bin/activate; \
        pip install --no-cache-dir -r src/python/requirements.txt || true; \
        # Replace ged2gwb with Python version if it exists \
        if [ -f "src/python/bin/ged2gwb" ]; then \
            echo "Replacing ged2gwb with Python version"; \
            cp src/python/bin/ged2gwb distribution/gw/ged2gwb; \
            chmod +x distribution/gw/ged2gwb; \
        fi; \
        # Copy Python environment \
        cp -r /home/opam/python-env distribution/python-env; \
        cp -r src/python distribution/python-src; \
    else \
        echo "🐪 Using OCaml binaries (fallback)"; \
    fi

###############################################################################
#                                       STAGE 2: Export build via blank image
###############################################################################

FROM scratch AS export
COPY --from=builder /home/opam/geneweb/distribution /

###############################################################################
#                                              STAGE 3: Assemble Docker image
###############################################################################

FROM debian:13-slim AS container

# Pass build argument to runtime
ARG USE_PYTHON_BINARIES=false
ENV USE_PYTHON_BINARIES=$USE_PYTHON_BINARIES

ENV GENEWEB_HOME=/usr/local/share/geneweb
ENV GENEWEB_DATA_PATH=${GENEWEB_HOME}/share/data
ENV GWSETUP_IP=172.17.0.1

# Install runtime tools (including Python if needed) and add Geneweb user
# Ignore the apt warning here as apt-get does not allow wildcarding versions
# hadolint ignore=DL3027
RUN apt-get update -q \
  && apt install -qy --no-install-recommends sudo openssl adduser \
  $([ "$USE_PYTHON_BINARIES" = "true" ] && echo "python3 python3-venv" || echo "") \
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

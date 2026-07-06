# Installation instructions

The GeneWeb project offers three binary distributions for user convenience.
- A dynamically linked distribution,
- A statically linked distribution,
- Docker images for Linux,

> [!WARNING]
> **Export backups of your databases in `.gw` format before installing a new version!**

# (RECOMMENDED) Dynamically linked distribution

The recommended installation procedure uses a tarball of binary artifacts
generated with oui. The only runtime dependencies are the system libraries.

This procedure is available for the following operating systems and architectures:
| OS/Architecture   | amd64 | arm64 |
| ----------------- | ----- | ----- |
| GNU/Linux         |   🟢  |  🟢   |
| macOS             |   🔴  |  🟢   |
| Windows           |   🟢  |  🔴   |

## Linux

The distribution is dynamically linked against the C libraries of a Debian LTS
distribution. It should work out of the box for sufficiently recent
distributions.

To install, run the self-extracting script as a non-root user:
```console
./geneweb-ARCH-dyn.run -- --prefix PREFIX
```
where `ARCH` is `amd64` or `arm64` and `PREFIX` is your choosen installation
directory.

This script installs the entire GeneWeb distribution into the `PREFIX` directory
and creates convenient symbolinks in the current user's home directory.

If the GeneWeb commands are not available in your shell afterward, you must add
`$HOME/.local/bin` to your PATH environment variable and `$HOME/.local/man` to
your MANPATH to access the manpages.

To uninstall a previous distribution, run:
```console
./PREFIX/geneweb/uninstall.sh
```

## MacOS

> [!TIP]
> GeneWeb man pages may contain UTF-8 characters. Make sure to enable
> UTF-8 support in your terminal.

```console
installer
```

The binary distribution is installed into `/Applications/geneweb`

## Windows

> [!TIP]
> GeneWeb man pages may contain UTF-8 characters. Make sure to enable
> UTF-8 support in your terminal.

The binary distribution is installed into `AppData\Local\Programs\geneweb`
directory.

# Statically linked distribution

The distribution is statically linked against the musl C library of Alpine
Linux distribution. It should work out of the box for any LTS distribution.

> [!WARNING]
> **This distribution doesn't use the C library of your system. The burden
> of keeping the C library updated falls on the user.**

| OS/Architecture   | amd64 | arm64 |
| ----------------- | ----- | ----- |
| GNU/Linux         |   🟢  |  🟢   |
| macOS             |   🔴  |  🔴   |
| Windows           |   🔴  |  🔴   |

# Docker images
GeneWeb projects distributes docker images.

```console
docker run -p 2317:2317 -v DIR:/bases -d -it geneweb-gwd:latest ARGS
```
where `DIR` is the directory of your databases and `ARGS` are the usual
arguments of `gwd`.

Run the following command
```console
docker logs -f CONTAINER
```
where `CONTAINER` is the name of gwd container to watch its logs.

```console
podman run -p 2317:2317 -v DIR:/bases -d --name geneweb-gwd:latest ARGS
```

# opam installation

GeneWeb is distributed on opam too. It is a source installation, which means
you need complete C toolchain to compile all the sources and the OCaml compiler.

Contrary to the binary distributions, the opam installion is home-wide, which
means relevant locations are in XDG directories. In particular,
- Databases directory is located in `$XDG_DATA_HOME/geneweb/bases`, which is
usually `$HOME/.share/geneweb/bases`.
- Configuration file is located in `$XDG_CONFIG_HOME/geneweb`.

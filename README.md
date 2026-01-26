<img align="left" src="hd/images/arbre_start.png" height="90" alt="GeneWeb">

# GeneWeb

## Open source genealogy software with a web interface written in OCaml
[![Build](https://github.com/geneweb/geneweb/actions/workflows/ci.yml/badge.svg)](https://github.com/geneweb/geneweb/actions/workflows/ci.yml)
[![Release](https://img.shields.io/github/v/release/geneweb/geneweb?label=latest)](https://github.com/geneweb/geneweb/releases/latest)
[![License](https://img.shields.io/badge/license-GPL--2.0-blue.svg)](LICENSE)

---

> [!NOTE]
> **Version 7.1 is currently in beta.**
> The software is stable and thoroughly tested. The "beta" label exists only
> because the internal database format is not yet compatible with Geneanet’s
> infrastructure. This will be resolved for the final 7.1.0 release.

---

## Why GeneWeb?

Created by [Daniel de Rauglaudre](https://github.com/roglo) in 1998, GeneWeb is the engine behind some of the largest genealogical databases
in the world:

- **[Roglo](https://roglo.eu)** — Over **11 million individuals**, maintained
  collaboratively by nearly **300 contributors**
- **[Geneanet](https://www.geneanet.org)** — Used GeneWeb as its core engine
  for two decades

What sets GeneWeb apart:

| Feature | Description |
|---------|-------------|
| **Proven at scale** | Handles databases with millions of individuals |
| **Battle-tested** | Years of development, real-world refinement |
| **Advanced relationships** | Find connections across billions of possible paths |
| **Your data, your control** | Run locally on your computer or deploy as a web server |
| **Privacy built-in** | Granular access controls protect living persons’ information |
| **Standard formats** | Full GEDCOM import and export for interoperability |

---

## Getting started

**Want to try first?** Run GeneWeb directly in your browser with
[Google Colab](https://github.com/geneweb/geneweb/blob/master/geneweb_colab.ipynb)
— no installation required.

> [!WARNING]
> **Export backups of your databases in `.gw` format before installing a new version!**

<details>
<summary><strong>Building from source</strong></summary>

You need [OCaml](https://ocaml.org) 4.10+ and
[opam](https://opam.ocaml.org).

```sh
# Install dependencies, configure and build
opam install . --deps-only
ocaml ./configure.ml
make distrib
```

See `ocaml ./configure.ml --help` for configuration options.

</details>

### Download

**[Download the latest release](https://github.com/geneweb/geneweb/releases/latest)**
for Linux, macOS, or Windows.

### First steps

1. **Extract** the downloaded archive to a folder of your choice
2. **Launch** the application:
   - Linux: run `./gwd.sh` in a terminal
   - macOS: double-click `geneweb.command`
   - Windows: double-click `START.htm`
3. **Open** your browser at [http://localhost:2317](http://localhost:2317)

You're ready for your genealogical journey.

<details>
<summary><strong>macOS security note</strong></summary>

macOS may block applications from unidentified developers. To authorize GeneWeb:

1. Right-click on `gwd` and `gwsetup` in the `gw` folder
2. Select "Open" from the context menu
3. Click "Open" in the security dialog

This only needs to be done once. After that, `geneweb.command` will work
normally.

</details>

<details>
<summary><strong>Running on port 80</strong></summary>

On Unix systems, ports below 1024 require elevated privileges.

**Linux** — use capabilities:
```sh
sudo setcap 'cap_net_bind_service=+ep' gwd
./gwd -p 80
```

**macOS** — use `launchd` to redirect port 80 to 2317.

</details>

---

## Documentation

- **[User documentation](https://geneweb.tuxfamily.org/wiki/GeneWeb)** —
  Getting started guides, tutorials, and reference
- **[API reference](http://geneweb.github.io/geneweb/)** —
  Generated documentation for developers
- **[Architecture overview](https://geneweb.github.io/)** —
  Technical introduction (by [OCamlPro](https://ocamlpro.com/))

---

## Community

- **Forum**: [Geneanet GeneWeb forum](https://www.geneanet.org/forum/GeneWeb-85)
  (French and English)
- **Mailing list**: [geneweb@framalistes.org](https://framalistes.org/sympa/subscribe/geneweb)
- **IRC**: [#geneweb on Libera.Chat](irc://irc.libera.chat/geneweb)

Found a bug or have a feature request?
[Open an issue](https://github.com/geneweb/geneweb/issues) on GitHub.

---

## Contributing

We welcome contributions from developers and translators.
See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

---

## License

GeneWeb is free software distributed under the
[GNU General Public License v2](LICENSE).

Copyright © 1998–2011 [INRIA](https://github.com/inria/).

---

<h3 align="center">Preserving family history, one generation at a time.</h3>